{-# LANGUAGE DerivingVia #-}

module Groq.Internal where

import Control.Monad (forM_)
import Control.Monad.Error.Class
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Aeson qualified as Aeson
import Data.Default (Default (..))
import Data.Generics.Labels ()
import Data.Sequence
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import GHC.Generics
import Lens.Micro
import Lens.Micro.Extras
import Network.HTTP.Req
import System.Environment

import Groq.ChatCompletion
import Groq.Config (APIKey (..), GroqCfg)

type GroqTRep m = ExceptT GroqError (StateT GroqCtx m)

newtype GroqT m a = GroqT
  { runGroqT :: ExceptT GroqError (StateT GroqCtx m) a
  }
  deriving newtype (Applicative, Functor, Monad)
  deriving (MonadIO) via GroqTRep m
  deriving (MonadError GroqError) via GroqTRep m
  deriving (MonadState GroqCtx) via GroqTRep m

instance (MonadIO m) => MonadHttp (GroqT m) where
  handleHttpException e = throwError $ GroqError (Prelude.show e)

{- | Base URL: https://api.groq.com/openai/v1
@Note: Maybe this kind of thing should be moved to an internals module?
-}
groqBase :: Url Https
groqBase = https "api.groq.com" /: "openai" /: "v1"

data GroqCtx = GroqCtx
  { apiKey :: GroqAPIKey,
    chatCtx :: ChatCreateRequest,
    groqUrl :: Url Https
  }
  deriving (Generic, Show)

newtype GroqError = GroqError {_errMessage :: String}

instance Show GroqError where
  show = ("GroqError: " ++) . _errMessage

type GroqAPIKey = T.Text

loadApiKey :: (MonadIO m) => APIKey -> m T.Text
loadApiKey (FromEnv var) = fmap T.pack . liftIO $ getEnv var

-- | Attempts to load groq api key from GROQ_API_KEY
initGroq :: (MonadIO m) => GroqCfg -> m GroqCtx
initGroq cfg = do
  apiKey <- loadApiKey $ cfg ^. #apiKey -- The only reason it's monadic
  let
    groqUrl = groqBase
    setSystemPrompt =
      case cfg ^. #systemPrompt of
        Nothing -> Prelude.id
        Just p -> #messages .~ singleton (mkSysChatMessage p)
    chatCtx =
      def
        & #model .~ (cfg ^. #model)
        & #temperature .~ (cfg ^. #temperature)
        & #maxCompletionTokens .~ (cfg ^. #maxCompletionTokens)
        & #topP .~ (cfg ^. #topP)
        & setSystemPrompt
  pure $
    GroqCtx
      { chatCtx,
        apiKey,
        groqUrl
      }

upsertChatMessage :: ChatMessage -> GroqCtx -> GroqCtx
upsertChatMessage msg ctx = ctx & #chatCtx . #messages %~ (|> msg)

-- | Add a chat completeion response into the request state
registerResponse :: (Monad m) => ChatCompletion -> GroqT m ()
registerResponse msg = do
  let
    xs = msg ^. #choices
  forM_ xs $
    modify' . upsertChatMessage . view #message

-- | Add a chat message into the request state
registerRequest :: (Monad m) => ChatMessage -> GroqT m ()
registerRequest = modify' . upsertChatMessage

-- | Makes a request to chat completions with whatever is in the current request state
makeRequest :: (MonadIO m) => GroqT m ChatCompletion
makeRequest = do
  ctx <- get
  let
    url = view #groqUrl ctx /: "chat" /: "completions"
    opts =
      mconcat
        [ header "Authorization" $ "Bearer " <> encodeUtf8 (ctx ^. #apiKey),
          header "Content-Type" "application/json"
        ]
    reqBody = Aeson.toJSON $ ctx ^. #chatCtx
  r <-
    req
      POST
      url
      (ReqBodyJson reqBody)
      lbsResponse
      opts
  case Aeson.decode @ChatCompletion $ responseBody r of
    Nothing -> throwError $ GroqError "Error: Failed to parse chat completion response."
    Just chat -> pure chat
