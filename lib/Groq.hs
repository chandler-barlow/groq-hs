{-# LANGUAGE OverloadedStrings #-}

module Groq where

import Control.Monad.IO.Class (liftIO)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Req

import Groq.Types
  ( ChatRole(..)
  , ChatMessage(..)
  , ChatCreateRequest(..)
  , ChatCompletion(..)
  , ChatChoice(..)
  )

-- | Base URL: https://api.groq.com/openai/v1
groqBase :: Url Https
groqBase = https "api.groq.com" /: "openai" /: "v1"

-- | Call POST /openai/v1/chat/completions
groqChatCompletions
  :: MonadHttp m
  => Text               -- ^ API key (GROQ_API_KEY)
  -> ChatCreateRequest  -- ^ Request body
  -> m ChatCompletion   -- ^ Parsed JSON response
groqChatCompletions apiKey reqBody = do
  let url = groqBase /: "chat" /: "completions"
      opts =
        header "Authorization" ("Bearer " <> encodeUtf8 apiKey)
        <> header "Content-Type" "application/json"
  r <- req
        POST
        url
        (ReqBodyJson reqBody)
        jsonResponse
        opts
  pure (responseBody r)

-- | Build a simple ChatCreateRequest:
--   model = "llama-3.3-70b-versatile"
--   messages = [ { role=user, content="Explain the importance of fast language models" } ]
mkSimpleChatRequest :: ChatCreateRequest
mkSimpleChatRequest =
  ChatCreateRequest
    { ccrMessages            =
        [ ChatMessage
            { cmRole    = ChatRoleUser
            , cmContent = "Explain the importance of fast language models"
            , cmName    = Nothing
            }
        ]
    , ccrModel               = "llama-3.3-70b-versatile"
    , ccrCitationOptions     = Nothing
    , ccrCompoundCustom      = Nothing
    , ccrDisableToolValidation = Nothing
    , ccrDocuments           = Nothing
    , ccrExcludeDomains      = Nothing
    , ccrFrequencyPenalty    = Nothing
    , ccrFunctionCall        = Nothing
    , ccrFunctions           = Nothing
    , ccrIncludeDomains      = Nothing
    , ccrIncludeReasoning    = Nothing
    , ccrLogitBias           = Nothing
    , ccrLogprobs            = Nothing
    , ccrMaxCompletionTokens = Nothing
    , ccrMaxTokens           = Nothing
    , ccrMetadata            = Nothing
    , ccrN                   = Nothing
    , ccrParallelToolCalls   = Nothing
    , ccrPresencePenalty     = Nothing
    , ccrReasoningEffort     = Nothing
    , ccrReasoningFormat     = Nothing
    , ccrResponseFormat      = Nothing
    , ccrSearchSettings      = Nothing
    , ccrSeed                = Nothing
    , ccrServiceTier         = Nothing
    , ccrStop                = Nothing
    , ccrStore               = Nothing
    , ccrStream              = Nothing
    , ccrStreamOptions       = Nothing
    , ccrTemperature         = Nothing
    , ccrToolChoice          = Nothing
    , ccrTools               = Nothing
    , ccrTopLogprobs         = Nothing
    , ccrTopP                = Nothing
    , ccrUser                = Nothing
    }
