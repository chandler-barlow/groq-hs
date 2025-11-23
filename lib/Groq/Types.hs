{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Groq.Types (
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

    -- * Responses API
    TruncationStrategy (..),
    GroqResponseRequest (..),
    GroqResponseReasoning (..),
    GroqResponseTextConfig (..),
    GroqResponseUsage (..),
    GroqResponseUsageDetails (..),
    GroqResponseOutputItem (..), -- simple typed "message" output
    GroqResponse (..),

    -- * Audio
    AudioTranscriptionResponse (..),
    AudioTranslationResponse (..),

    -- * Models
    Model (..),
    ModelsList (..),

    -- * Batches
    BatchStatus (..),
    BatchRequestCounts (..),
    BatchObject (..),
    BatchesList (..),
    BatchCreateRequest (..),

    -- * Files
    FilePurpose (..),
    GroqFile (..),
    FilesList (..),
    FileDeleteResponse (..),

    -- * Fine Tuning
    FineTuningItem (..),
    FineTuningsList (..),
    FineTuningWrapper (..),
    FineTuningDeleteResponse (..),
    mkUserChatMessage,
    mkEmptyChatRequest,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Text (Text)
import GHC.Generics (Generic)

import Data.Default
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

--------------------------------------------------------------------------------
-- Responses API (/openai/v1/responses)
--------------------------------------------------------------------------------

data TruncationStrategy
    = TruncationAuto
    | TruncationDisabled
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 10) ''TruncationStrategy)

-- "auto", "disabled"

data GroqResponseReasoning = GroqResponseReasoning
    { grrEffort :: Maybe ReasoningEffort
    , grrSummary :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''GroqResponseReasoning)

newtype GroqResponseTextConfig = GroqResponseTextConfig
    { grtFormat :: Value
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''GroqResponseTextConfig)

-- | Request body for POST /openai/v1/responses
data GroqResponseRequest = GroqResponseRequest
    { grqInput :: Value -- string / array / other
    , grqModel :: Text
    , grqInstructions :: Maybe Text
    , grqMaxOutputTokens :: Maybe Int
    , grqMetadata :: Maybe Value
    , grqParallelToolCalls :: Maybe Bool
    , grqReasoning :: Maybe Value
    , grqServiceTier :: Maybe ServiceTier
    , grqStore :: Maybe Bool
    , grqStream :: Maybe Bool
    , grqTemperature :: Maybe Double
    , grqText :: Maybe GroqResponseTextConfig
    , grqToolChoice :: Maybe Value
    , grqTools :: Maybe [Value]
    , grqTopP :: Maybe Double
    , grqTruncation :: Maybe TruncationStrategy
    , grqUser :: Maybe Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''GroqResponseRequest)

-- A convenient typed view of a simple "message" output item.
data GroqResponseOutputItem = GroqResponseOutputItem
    { groiType :: Text
    , groiId :: Maybe Text
    , groiStatus :: Maybe Text
    , groiRole :: Maybe Text
    , groiContent :: Maybe Value
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 4) ''GroqResponseOutputItem)

data GroqResponseUsageDetails = GroqResponseUsageDetails
    { gruduCachedTokens :: Maybe Int
    , gruduReasoningTokens :: Maybe Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 5) ''GroqResponseUsageDetails)

data GroqResponseUsage = GroqResponseUsage
    { gruInputTokens :: Maybe Int
    , gruInputTokensDetails :: Maybe GroqResponseUsageDetails
    , gruOutputTokens :: Maybe Int
    , gruOutputTokensDetails :: Maybe GroqResponseUsageDetails
    , gruTotalTokens :: Maybe Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''GroqResponseUsage)

-- | Response for POST /openai/v1/responses
data GroqResponse = GroqResponse
    { grId :: Text
    , grObject :: Text -- "response"
    , grStatus :: Text -- completed | failed | ...
    , grCreatedAt :: Int
    , grOutput :: [GroqResponseOutputItem]
    , grPreviousResponseId :: Maybe Text
    , grModel :: Text
    , grReasoning :: Maybe GroqResponseReasoning
    , grMaxOutputTokens :: Maybe Int
    , grInstructions :: Maybe Text
    , grText :: Maybe GroqResponseTextConfig
    , grTools :: [Value]
    , grToolChoice :: Value
    , grTruncation :: TruncationStrategy
    , grMetadata :: Value
    , grTemperature :: Double
    , grTopP :: Double
    , grUser :: Maybe Text
    , grServiceTier :: ServiceTier
    , grError :: Maybe Value
    , grIncompleteDetails :: Maybe Value
    , grUsage :: GroqResponseUsage
    , grParallelToolCalls :: Bool
    , grStore :: Bool
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''GroqResponse)

--------------------------------------------------------------------------------
-- Audio (transcription / translation / TTS)
--------------------------------------------------------------------------------

-- | Response for audio transcription: { "text": "...", "x_groq": { "id": ... } }
data AudioTranscriptionResponse = AudioTranscriptionResponse
    { atrText :: Text
    , atrXGroq :: Maybe ChatXGroq
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''AudioTranscriptionResponse)

-- | Response for audio translation: { "text": "...", "x_groq": { "id": ... } }
data AudioTranslationResponse = AudioTranslationResponse
    { atvText :: Text
    , atvXGroq :: Maybe ChatXGroq
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''AudioTranslationResponse)

-- NOTE: /audio/speech returns raw audio, so we don't define a JSON type for it.

--------------------------------------------------------------------------------
-- Models
--------------------------------------------------------------------------------

data Model = Model
    { moId :: Text
    , moObject :: Text -- "model"
    , moCreated :: Int
    , moOwnedBy :: Text
    , moActive :: Maybe Bool
    , moContextWindow :: Maybe Int
    , moPublicApps :: Maybe Value
    , moMaxCompletionTokens :: Maybe Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''Model)

data ModelsList = ModelsList
    { mlObject :: Text -- "list"
    , mlData :: [Model]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''ModelsList)

--------------------------------------------------------------------------------
-- Batches
--------------------------------------------------------------------------------

data BatchStatus
    = BatchStatusValidating
    | BatchStatusFailed
    | BatchStatusInProgress
    | BatchStatusFinalizing
    | BatchStatusCompleted
    | BatchStatusExpired
    | BatchStatusCancelling
    | BatchStatusCancelled
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 11) ''BatchStatus)

-- "validating", "failed", ...

data BatchRequestCounts = BatchRequestCounts
    { brcTotal :: Int
    , brcCompleted :: Int
    , brcFailed :: Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''BatchRequestCounts)

-- | Full batch object (used by create / retrieve / list / cancel)
data BatchObject = BatchObject
    { boId :: Text
    , boObject :: Text -- "batch"
    , boEndpoint :: Text
    , boErrors :: Maybe Value
    , boInputFileId :: Text
    , boCompletionWindow :: Text
    , boStatus :: BatchStatus
    , boOutputFileId :: Maybe Text
    , boErrorFileId :: Maybe Text
    , boFinalizingAt :: Maybe Int
    , boFailedAt :: Maybe Int
    , boExpiredAt :: Maybe Int
    , boCancelledAt :: Maybe Int
    , boRequestCounts :: BatchRequestCounts
    , boMetadata :: Maybe Value
    , boCreatedAt :: Int
    , boExpiresAt :: Int
    , boCancellingAt :: Maybe Int
    , boCompletedAt :: Maybe Int
    , boInProgressAt :: Maybe Int
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''BatchObject)

data BatchesList = BatchesList
    { blObject :: Text
    , blData :: [BatchObject]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''BatchesList)

-- | Request body for POST /openai/v1/batches
data BatchCreateRequest = BatchCreateRequest
    { bcrCompletionWindow :: Text
    , bcrEndpoint :: Text -- "/v1/chat/completions"
    , bcrInputFileId :: Text
    , bcrMetadata :: Maybe Value
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''BatchCreateRequest)

--------------------------------------------------------------------------------
-- Files
--------------------------------------------------------------------------------

data FilePurpose
    = FilePurposeBatch
    | FilePurposeBatchOutput
    deriving (Show, Eq, Ord, Generic)

$(deriveJSON (sumOptions 11) ''FilePurpose)

-- "batch", "batch_output"

data GroqFile = GroqFile
    { gfId :: Text
    , gfObject :: Text -- "file"
    , gfBytes :: Int
    , gfCreatedAt :: Int
    , gfFilename :: Text
    , gfPurpose :: FilePurpose
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''GroqFile)

data FilesList = FilesList
    { flObject :: Text
    , flData :: [GroqFile]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 2) ''FilesList)

data FileDeleteResponse = FileDeleteResponse
    { fdrId :: Text
    , fdrObject :: Text -- "file"
    , fdrDeleted :: Bool
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''FileDeleteResponse)

--------------------------------------------------------------------------------
-- Fine Tunings (closed beta)
--------------------------------------------------------------------------------

data FineTuningItem = FineTuningItem
    { ftiId :: Text
    , ftiName :: Text
    , ftiBaseModel :: Text
    , ftiType :: Text
    , ftiInputFileId :: Text
    , ftiCreatedAt :: Int
    , ftiFineTunedModel :: Text
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''FineTuningItem)

data FineTuningsList = FineTuningsList
    { ftlObject :: Text
    , ftlData :: [FineTuningItem]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''FineTuningsList)

-- Wrapper used by create / get endpoints:
-- { "id": "...", "object": "...", "data": { FineTuningItem } }
data FineTuningWrapper = FineTuningWrapper
    { ftwId :: Text
    , ftwObject :: Text
    , ftwData :: FineTuningItem
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''FineTuningWrapper)

data FineTuningDeleteResponse = FineTuningDeleteResponse
    { ftdId :: Text
    , ftdObject :: Text -- "fine_tuning"
    , ftdDeleted :: Bool
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 3) ''FineTuningDeleteResponse)

instance Default ChatMessage where
    def =
        ChatMessage
            { cmRole = undefined
            , cmContent = undefined
            , cmName = undefined
            }
