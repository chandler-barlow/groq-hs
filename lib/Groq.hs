{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingVia #-}

module Groq (
    prompt, 
    execGroq, 
    GroqCfg(..),
    APIKey(..)
 ) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Generics.Labels ()
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Groq.Types.ChatCompletion (
    ChatCompletion (..),
    ChatCreateRequest (..),
    ChatMessage (..),
    ReasoningEffort (..),
 )
import Lens.Micro ((&), (.~), (^.), (?~), (%~))
import Network.HTTP.Req

import Data.Aeson qualified as Aeson
import GHC.Generics (Generic)
import Groq.Types.Models
import System.Environment (getEnv)
import Data.Default (Default (..))
import Control.Monad.State (
    StateT (..), 
    MonadState, 
    modify'
 )
import Control.Monad.RWS.Lazy (MonadState(..))
import Control.Monad (forM_)
import Lens.Micro.Extras (view)
import Data.Maybe (listToMaybe)

{- |
    I am thinking that this module should actually be split into
    Groq.Internal and Groq.Simple.

    I think that an api like this would be great.

    @@
    runGroq cfg $ do
        res <- chat "What is the history of AI in a sentence?"
        liftIO $ print res
    @@

    Maybe tools could be added like this?

    @@
    let cfg = def{tools = myTools}
    runGroq cfg $ do
        res <- chat "invoke your favorite tool"
        liftIO $ print res
    @@

    I think that the chat completion request actually manifests as a form of configuration
-}

{- | Base URL: https://api.groq.com/openai/v1
@Note: Maybe this kind of thing should be moved to an internals module?
-}
groqBase :: Url Https
groqBase = https "api.groq.com" /: "openai" /: "v1"

{- | @TODO add tools!
Basically required for this to be usable for agents
-}
data GroqCtx = GroqCtx
    { apiKey :: GroqAPIKey
    , chatCtx :: ChatCreateRequest
    {- ^ request that we keep re-using
    This obj should be updated after each call
    -}
    , groqUrl :: Url Https
    }
    deriving (Generic)

-- | Maybe we will want other options later?
newtype APIKey = FromEnv String

{- |
    We should provide some level of
    initial configuration for the initial request.
    All defaults come from groq cloud studio.
-}
data GroqCfg = GroqCfg
    { model :: ModelId
    , temperature :: Double
    , reasoningEffort :: ReasoningEffort
    , maxCompletionTokens :: Int
    , stream :: Bool
    , topP :: Double
    , apiKey :: APIKey
    }
    deriving (Generic)

instance Default GroqCfg where
    def =
        GroqCfg
            { model = Model_groq_compound_mini
            , temperature = 0.6
            , reasoningEffort = ReasoningEffortDefault
            , maxCompletionTokens = 4096
            , stream = True
            , topP = 0.95
            , apiKey = FromEnv "GROQ_API_KEY"
            }

newtype GroqError = GroqError {_errMessage :: String}

type GroqAPIKey = Text

{- |
    Transformer for managing errors and chat state.
    Using this it's technically possible to tweak any of the
    chat settings during run time.

    Maybe I can expose that later?
-}

type GroqTRep m = ExceptT GroqError (StateT GroqCtx m) 

newtype GroqT m a = GroqT
    { runGroqT :: ExceptT GroqError (StateT GroqCtx m) a
    }
    deriving newtype (Functor, Applicative, Monad)
    deriving MonadIO via GroqTRep m
    deriving (MonadError GroqError) via GroqTRep m
    deriving (MonadState GroqCtx) via GroqTRep m
    deriving MonadHttp via GroqTRep m

-- TODO handle errors
loadApiKey :: (MonadIO m) => APIKey -> m Text
loadApiKey (FromEnv var) = fmap T.pack . liftIO $ getEnv var

{- | Attempts to load groq api key from GROQ_API_KEY
TODO think about adding fallbacks or settings
-}
initGroq :: (MonadIO m) => GroqCfg -> m GroqCtx
initGroq cfg = do
    apiKey <- loadApiKey $ cfg ^. #apiKey -- The only reason it's monadic
    let groqUrl = groqBase
        chatCtx =
            def
                & #model .~ (cfg ^. #model)
                & #temperature ?~ (cfg ^. #temperature)
                & #reasoningEffort ?~ (cfg ^. #reasoningEffort)
                & #maxCompletionTokens ?~ (cfg ^. #maxCompletionTokens)
                & #stream ?~ (cfg ^. #stream)
                & #topP ?~ (cfg ^. #topP)
    pure $
        GroqCtx
            { chatCtx
            , apiKey
            , groqUrl
            }

execGroq :: (MonadIO m) => GroqCfg -> GroqT m a -> m (Either GroqError a)
execGroq cfg fn = do
    ctx <- initGroq cfg
    fmap fst 
        . flip runStateT ctx 
        . runExceptT 
        $ runGroqT fn

upsertChatMessage :: ChatMessage -> GroqCtx -> GroqCtx
upsertChatMessage msg ctx = ctx & #chatCtx . #messages %~ (msg :)

-- | Add a chat completeion response into the request state
registerResponse :: Monad m => ChatCompletion -> GroqT m ()
registerResponse msg = do
    let xs = msg ^. #choices
    forM_ xs $
        modify' . upsertChatMessage . view #message

-- | Add a chat message into the request state
registerRequest :: Monad m => ChatMessage -> GroqT m ()
registerRequest = modify' . upsertChatMessage

-- | Makes a request to chat completions with whatever is in the current request state
makeRequest :: MonadHttp m => GroqT m ChatCompletion
makeRequest = do
    ctx <- get
    let url = view #groqUrl ctx /: "chat" /: "completions"
        opts =
            mconcat
                [ header "Authorization" $ "Bearer " <> encodeUtf8 (ctx ^. #apiKey)
                , header "Content-Type" "application/json"
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

-- | Send a prompt to groq as a user
prompt :: (MonadIO m, MonadHttp m) => Text -> GroqT m Text
prompt msg = do
    let request = def & #content .~ msg
    registerRequest request
    response <- makeRequest
    registerResponse response
    let rs = response ^. #choices
        rs' = view (#message .  #content) <$> listToMaybe rs
    case rs' of
        Nothing -> throwError $ GroqError "Error: Absurd, no request was ever made"
        Just x -> pure x
