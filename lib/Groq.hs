{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Groq where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Groq.Types (
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

-- | Base URL: https://api.groq.com/openai/v1
-- @Note: Maybe this kind of thing should be moved to an internals module?
groqBase :: Url Https
groqBase = https "api.groq.com" /: "openai" /: "v1"

{- | @TODO add tools!
Basically required for this to be usable for agents
-}
data GroqCtx = GroqCtx
    { apiKey :: Text
    , chatHistory :: [Groq.Types.ChatMessage]
    , groqUrl :: Url Https
    }

{- | Attempts to load groq api key from GROQ_API_KEY
TODO think about adding fallbacks or settings
-}
initGroq :: (MonadIO m) => m GroqCtx
initGroq = do
    apiKey <- fmap T.pack . liftIO $ getEnv "GROQ_API_KEY"
    let chatHistory = mempty
        groqUrl = groqBase
    pure $ GroqCtx{..}

newtype GroqError = GroqError {errMessage :: String}

type GroqAPIKey = Text

chatCompletionRequest ::
    (MonadHttp m) =>
    GroqAPIKey ->
    Url Https ->
    Groq.Types.ChatCreateRequest ->
    m (Either GroqError Groq.Types.ChatCompletion)
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

    case Aeson.decode @Groq.Types.ChatCompletion $ responseBody r of
        Nothing -> pure . Left $ GroqError "Error: Failed to parse chat completion response."
        Just chat -> pure . Right $ chat

groqChat ::
    (MonadHttp m) =>
    GroqCtx ->
    Groq.Types.ChatMessage ->
    m (Either GroqError (GroqCtx, Groq.Types.ChatCompletion))
groqChat ctx msg = do
    let chatHistory' = msg : ctx.chatHistory
        chatReq =
            Groq.Types.mkEmptyChatRequest
                { ccrMessages = chatHistory'
                }
    runExceptT $ do
        chat <- ExceptT $ chatCompletionRequest ctx.apiKey ctx.groqUrl chatReq
        pure (ctx{chatHistory = chatHistory'}, chat)
