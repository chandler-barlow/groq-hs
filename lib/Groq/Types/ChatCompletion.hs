{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Groq.Types.ChatCompletion (
    -- * Chat completions
    ChatRole (..),
    ServiceTier (..),
    ReasoningEffort (..),
    ReasoningFormat (..),
    CitationOptions (..),
    ChatMessage (..),
    ChatCreateRequest (..),
    ChatChoice (..),
    ChatUsage (..),
    ChatUsageBreakdown (..),
    ChatXGroq (..),
    ChatCompletion (..),
    mkUserChatMessage,
    mkSysChatMessage,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics (Generic)

import Groq.Types.Models
import Utils
import Data.Default

data ChatRole
    = ChatRoleUser
    | ChatRoleAssistant
    | ChatRoleSystem
    | ChatRoleTool
    | ChatRoleFunction
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 8) ''ChatRole)

data ServiceTier
    = ServiceTierAuto
    | ServiceTierOnDemand
    | ServiceTierFlex
    | ServiceTierPerformance
    | ServiceTierDefault
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 11) ''ServiceTier)

data ReasoningEffort
    = ReasoningEffortNone
    | ReasoningEffortDefault
    | ReasoningEffortLow
    | ReasoningEffortMedium
    | ReasoningEffortHigh
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 15) ''ReasoningEffort)

data ReasoningFormat
    = ReasoningFormatHidden
    | ReasoningFormatRaw
    | ReasoningFormatParsed
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 15) ''ReasoningFormat)

data CitationOptions
    = CitationOptionsEnabled
    | CitationOptionsDisabled
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 15) ''CitationOptions)

data ChatMessage = ChatMessage
    { role :: ChatRole
    , content :: Text
    , name :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatMessage)

-- | Creates a user chat message with some defaults
mkUserChatMessage :: Text -> ChatMessage
mkUserChatMessage msg =
    ChatMessage
        { role = ChatRoleUser
        , content = msg
        , name = Nothing
        }

-- | Allows for system prompts
mkSysChatMessage :: Text -> ChatMessage
mkSysChatMessage msg =
    ChatMessage
        { role = ChatRoleSystem
        , content = msg
        , name = Nothing
        }

{- |
    Request body for POST /openai/v1/chat/completions

    @Warning:
    Many of these are unimplemented but theoretically possible to use.
    Highest priority would be adding tools I think.
    Only messages really work at the moment
-}
data ChatCreateRequest = ChatCreateRequest
    { messages :: [ChatMessage]
    , model :: ModelId
    , citationOptions :: Maybe CitationOptions
    , compoundCustom :: Maybe Value
    -- ^ TODO
    , disableToolValidation :: Maybe Bool
    , documents :: Maybe [Value]
    -- ^ TODO
    , excludeDomains :: Maybe [Text]
    , frequencyPenalty :: Maybe Double
    , functionCall :: Maybe Value -- deprecated in favor of tool_choice

    -- ^ TODO: low priority
    , functions :: Maybe [Value] -- deprecated in favor of tools

    -- ^ TODO: low priority
    , includeDomains :: Maybe [Text]
    , includeReasoning :: Maybe Bool
    , logitBias :: Maybe Value
    -- ^ TODO
    , logprobs :: Maybe Bool
    , maxCompletionTokens :: Maybe Int
    , maxTokens :: Maybe Int -- deprecated
    , metadata :: Maybe Value
    -- ^ TODO
    , n :: Maybe Int
    , parallelToolCalls :: Maybe Bool
    , presencePenalty :: Maybe Double
    , reasoningEffort :: Maybe ReasoningEffort
    , reasoningFormat :: Maybe ReasoningFormat
    , responseFormat :: Maybe Value
    -- ^ TODO
    , searchSettings :: Maybe Value
    -- ^ TODO
    , seed :: Maybe Int
    , serviceTier :: Maybe ServiceTier
    , stop :: Maybe Value -- string or array

    -- ^ TODO
    , store :: Maybe Bool
    , stream :: Maybe Bool
    , streamOptions :: Maybe Value
    -- ^ TODO
    , temperature :: Maybe Double
    , toolChoice :: Maybe Value
    -- ^ TODO: These are high value
    , tools :: Maybe [Value]
    -- ^ TODO: These are high value
    , topLogprobs :: Maybe Int
    , topP :: Maybe Double
    , user :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''ChatCreateRequest)

-- | Uses groq compound mini by default and includes no message
instance Default ChatCreateRequest where
    def =
        ChatCreateRequest
            { messages = []
            , model = def
            , citationOptions = Nothing
            , compoundCustom = Nothing
            , disableToolValidation = Nothing
            , documents = Nothing
            , excludeDomains = Nothing
            , frequencyPenalty = Nothing
            , functionCall = Nothing
            , functions = Nothing
            , includeDomains = Nothing
            , includeReasoning = Nothing
            , logitBias = Nothing
            , logprobs = Nothing
            , maxCompletionTokens = Nothing
            , maxTokens = Nothing
            , metadata = Nothing
            , n = Nothing
            , parallelToolCalls = Nothing
            , presencePenalty = Nothing
            , reasoningEffort = Nothing
            , reasoningFormat = Nothing
            , responseFormat = Nothing
            , searchSettings = Nothing
            , seed = Nothing
            , serviceTier = Nothing
            , stop = Nothing
            , store = Nothing
            , stream = Nothing
            , streamOptions = Nothing
            , temperature = Nothing
            , toolChoice = Nothing
            , tools = Nothing
            , topLogprobs = Nothing
            , topP = Nothing
            , user = Nothing
            }

data ChatChoice = ChatChoice
    { index :: Int
    , message :: ChatMessage
    , logprobs :: Maybe Value
    , finishReason :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatChoice)

data ChatUsageBreakdown = ChatUsageBreakdown
    { promptTokens :: Maybe Int
    , completionTokens :: Maybe Int
    , totalTokens :: Maybe Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatUsageBreakdown)

data ChatUsage = ChatUsage
    { queueTime :: Maybe Double
    , promptTokens :: Maybe Int
    , promptTime :: Maybe Double
    , completionTokens :: Maybe Int
    , completionTime :: Maybe Double
    , totalTokens :: Maybe Int
    , totalTime :: Maybe Double
    , usageBreakdown :: Maybe ChatUsageBreakdown
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatUsage)

newtype ChatXGroq = ChatXGroq
    { cxId :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''ChatXGroq)

-- | Response for POST /openai/v1/chat/completions
data ChatCompletion = ChatCompletion
    { id :: Text
    , object :: Text
    , created :: Int
    , model :: Text
    , choices :: [ChatChoice]
    , usage :: Maybe ChatUsage
    , systemFingerprint :: Maybe Text
    , serviceTier :: Maybe ServiceTier
    , xGroq :: Maybe ChatXGroq
    , mcpListTools :: Maybe [Value]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatCompletion)
