{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Groq where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Groq.Types.ChatCompletion (
    ChatCompletion (..),
    ChatCreateRequest (..),
    ChatMessage (..),
    mkEmptyChatRequest,
 )
import Network.HTTP.Req

import Data.Aeson qualified as Aeson
import System.Environment (getEnv)

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

-- TODO handle errors
loadApiKey :: (MonadIO m) => APIKey -> m Text
loadApiKey (FromEnv var) = fmap T.pack . liftIO $ getEnv "GROQ_API_KEY"

{- | Attempts to load groq api key from GROQ_API_KEY
TODO think about adding fallbacks or settings
-}
initGroq :: (MonadIO m) => GroqCfg -> m GroqCtx
initGroq cfg = do
    apiKey <- loadApiKey cfg.apiKey
    let chatCtx =
            def
                { model = cfg.model
                , temperature = cfg.temperature
                , reasoningEffort = cfg.reasoningEffort
                , maxCompletionTokens = cfg.maxCompletionTokens
                , stream = cfg.stream
                , topP = cfg.topP
                }
        groqUrl = groqBase
    pure $
        GroqCtx
            { chatCtx
            , apiKey
            , groqUrl
            }

newtype GroqError = GroqError {errMessage :: String}

type GroqAPIKey = Text

-- | Low level request for chat/prompt
chatCompletionRequest ::
    (MonadHttp m) =>
    GroqAPIKey ->
    Url Https ->
    ChatCreateRequest ->
    m (Either GroqError ChatCompletion)
chatCompletionRequest apiKey groqUrl chatRequest = do
    let url = groqUrl /: "chat" /: "completions"
        opts =
            mconcat
                [ header "Authorization" $ "Bearer " <> encodeUtf8 apiKey
                , header "Content-Type" "application/json"
                ]
        reqBody = Aeson.toJSON chatRequest
    r <-
        req
            POST
            url
            (ReqBodyJson reqBody)
            lbsResponse
            opts

    case Aeson.decode @ChatCompletion $ responseBody r of
        Nothing -> pure . Left $ GroqError "Error: Failed to parse chat completion response."
        Just chat -> pure . Right $ chat

groqChat ::
    (MonadHttp m) =>
    GroqCtx ->
    ChatMessage ->
    m (Either GroqError (GroqCtx, ChatCompletion))
groqChat ctx msg = do
    let chatHistory' = msg : ctx.chatHistory
        chatReq =
            mkEmptyChatRequest
                { ccrMessages = chatHistory'
                }
    runExceptT $ do
        chat <- ExceptT $ chatCompletionRequest ctx.apiKey ctx.groqUrl chatReq
        pure (ctx{chatHistory = chatHistory'}, chat)

{- |
    Transformer for managing errors and chat state.
    Using this it's technically possible to tweak any of the
    chat settings during run time.

    Maybe I can expose that later?
-}
newtype GroqT m a = GroqT
    { runGroqT :: ExceptT GroqError (StateT m GroqCtx) a
    }

-- I think I could have derived these somehow
instance (Monad m) => MonadState GroqCtx (GroqT m) where
    get = GroqT . ExceptT . fmap Right . StateT $ \s -> pure (s, s)
    put s = GroqT . ExceptT . fmap Right . StateT . const $ pure ((), s)

instance (Monad m) => MonadError GroqError (GroqT m) where
    throwError = GroqT . throwError
    catchError (GroqT m) = GroqT . catchError m

execGroq :: (MonadIO m) => GroqCfg -> GroqT m a -> m a
execGroq = undefined

-- | Send a prompt to groq and then get a response
prompt :: (MonadIO m) => Text -> GroqT m Text
prompt = undefined
