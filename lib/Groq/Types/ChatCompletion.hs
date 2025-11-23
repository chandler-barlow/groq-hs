{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Groq.ChatCompletion (
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
    mkEmptyChatRequest,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics (Generic)

import Groq.Models
import Utils

--------------------------------------------------------------------------------
-- Chat completions
--------------------------------------------------------------------------------

data ChatRole
    = ChatRoleUser
    | ChatRoleAssistant
    | ChatRoleSystem
    | ChatRoleTool
    | ChatRoleFunction
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 8) ''ChatRole)

-- "user", "assistant", "system", "tool", "function"

data ServiceTier
    = ServiceTierAuto
    | ServiceTierOnDemand
    | ServiceTierFlex
    | ServiceTierPerformance
    | ServiceTierDefault
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 11) ''ServiceTier)

-- "auto", "on_demand", "flex", "performance", "default"

data ReasoningEffort
    = ReasoningEffortNone
    | ReasoningEffortDefault
    | ReasoningEffortLow
    | ReasoningEffortMedium
    | ReasoningEffortHigh
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 15) ''ReasoningEffort)

-- "none", "default", "low", "medium", "high"

data ReasoningFormat
    = ReasoningFormatHidden
    | ReasoningFormatRaw
    | ReasoningFormatParsed
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 15) ''ReasoningFormat)

-- "hidden", "raw", "parsed"

data CitationOptions
    = CitationOptionsEnabled
    | CitationOptionsDisabled
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 15) ''CitationOptions)

data ChatMessage = ChatMessage
    { cmRole :: ChatRole
    , cmContent :: Text
    , cmName :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''ChatMessage)

-- | Creates a user chat message with some defaults
mkUserChatMessage :: Text -> ChatMessage
mkUserChatMessage msg =
    ChatMessage
        { cmRole = ChatRoleUser
        , cmContent = msg
        , cmName = Nothing
        }

{- |
    Request body for POST /openai/v1/chat/completions

    @Warning:
    Many of these are unimplemented but theoretically possible to use.
    Highest priority would be adding tools I think.
    Only messages really work at the moment
-}
data ChatCreateRequest = ChatCreateRequest
    { ccrMessages :: [ChatMessage]
    , ccrModel :: ModelId
    , ccrCitationOptions :: Maybe CitationOptions
    , ccrCompoundCustom :: Maybe Value
    -- ^ TODO
    , ccrDisableToolValidation :: Maybe Bool
    , ccrDocuments :: Maybe [Value]
    -- ^ TODO
    , ccrExcludeDomains :: Maybe [Text]
    , ccrFrequencyPenalty :: Maybe Double
    , ccrFunctionCall :: Maybe Value -- deprecated in favor of tool_choice

    -- ^ TODO: low priority
    , ccrFunctions :: Maybe [Value] -- deprecated in favor of tools

    -- ^ TODO: low priority
    , ccrIncludeDomains :: Maybe [Text]
    , ccrIncludeReasoning :: Maybe Bool
    , ccrLogitBias :: Maybe Value
    -- ^ TODO
    , ccrLogprobs :: Maybe Bool
    , ccrMaxCompletionTokens :: Maybe Int
    , ccrMaxTokens :: Maybe Int -- deprecated
    , ccrMetadata :: Maybe Value
    -- ^ TODO
    , ccrN :: Maybe Int
    , ccrParallelToolCalls :: Maybe Bool
    , ccrPresencePenalty :: Maybe Double
    , ccrReasoningEffort :: Maybe ReasoningEffort
    , ccrReasoningFormat :: Maybe ReasoningFormat
    , ccrResponseFormat :: Maybe Value
    -- ^ TODO
    , ccrSearchSettings :: Maybe Value
    -- ^ TODO
    , ccrSeed :: Maybe Int
    , ccrServiceTier :: Maybe ServiceTier
    , ccrStop :: Maybe Value -- string or array

    -- ^ TODO
    , ccrStore :: Maybe Bool
    , ccrStream :: Maybe Bool
    , ccrStreamOptions :: Maybe Value
    -- ^ TODO
    , ccrTemperature :: Maybe Double
    , ccrToolChoice :: Maybe Value
    -- ^ TODO: These are high value
    , ccrTools :: Maybe [Value]
    -- ^ TODO: These are high value
    , ccrTopLogprobs :: Maybe Int
    , ccrTopP :: Maybe Double
    , ccrUser :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''ChatCreateRequest)

-- | Uses groq compound mini by default and includes no message
mkEmptyChatRequest :: ChatCreateRequest
mkEmptyChatRequest =
    ChatCreateRequest
        { ccrMessages = []
        , ccrModel = Model_groq_compound_mini
        , ccrCitationOptions = Nothing
        , ccrCompoundCustom = Nothing
        , ccrDisableToolValidation = Nothing
        , ccrDocuments = Nothing
        , ccrExcludeDomains = Nothing
        , ccrFrequencyPenalty = Nothing
        , ccrFunctionCall = Nothing
        , ccrFunctions = Nothing
        , ccrIncludeDomains = Nothing
        , ccrIncludeReasoning = Nothing
        , ccrLogitBias = Nothing
        , ccrLogprobs = Nothing
        , ccrMaxCompletionTokens = Nothing
        , ccrMaxTokens = Nothing
        , ccrMetadata = Nothing
        , ccrN = Nothing
        , ccrParallelToolCalls = Nothing
        , ccrPresencePenalty = Nothing
        , ccrReasoningEffort = Nothing
        , ccrReasoningFormat = Nothing
        , ccrResponseFormat = Nothing
        , ccrSearchSettings = Nothing
        , ccrSeed = Nothing
        , ccrServiceTier = Nothing
        , ccrStop = Nothing
        , ccrStore = Nothing
        , ccrStream = Nothing
        , ccrStreamOptions = Nothing
        , ccrTemperature = Nothing
        , ccrToolChoice = Nothing
        , ccrTools = Nothing
        , ccrTopLogprobs = Nothing
        , ccrTopP = Nothing
        , ccrUser = Nothing
        }

data ChatChoice = ChatChoice
    { chcIndex :: Int
    , chcMessage :: ChatMessage
    , chcLogprobs :: Maybe Value
    , chcFinishReason :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''ChatChoice)

data ChatUsageBreakdown = ChatUsageBreakdown
    { cubPromptTokens :: Maybe Int
    , cubCompletionTokens :: Maybe Int
    , cubTotalTokens :: Maybe Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''ChatUsageBreakdown)

data ChatUsage = ChatUsage
    { cuQueueTime :: Maybe Double
    , cuPromptTokens :: Maybe Int
    , cuPromptTime :: Maybe Double
    , cuCompletionTokens :: Maybe Int
    , cuCompletionTime :: Maybe Double
    , cuTotalTokens :: Maybe Int
    , cuTotalTime :: Maybe Double
    , cuUsageBreakdown :: Maybe ChatUsageBreakdown
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''ChatUsage)

newtype ChatXGroq = ChatXGroq
    { cxId :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''ChatXGroq)

-- | Response for POST /openai/v1/chat/completions
data ChatCompletion = ChatCompletion
    { ccId :: Text
    , ccObject :: Text
    , ccCreated :: Int
    , ccModel :: Text
    , ccChoices :: [ChatChoice]
    , ccUsage :: Maybe ChatUsage
    , ccSystemFingerprint :: Maybe Text
    , ccServiceTier :: Maybe ServiceTier
    , ccXGroq :: Maybe ChatXGroq
    , ccMcpListTools :: Maybe [Value]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''ChatCompletion)
