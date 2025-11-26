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
import Data.Sequence (Seq)

data ChatRole
    = ChatRoleUser
    | ChatRoleAssistant
    | ChatRoleSystem
    | ChatRoleTool
    | ChatRoleFunction
    deriving (Show, Eq, Ord, Generic)

instance Default ChatRole where
    def = ChatRoleUser

$(deriveJsonEnum 8 ''ChatRole)

data ServiceTier
    = ServiceTierAuto
    | ServiceTierOnDemand
    | ServiceTierFlex
    | ServiceTierPerformance
    | ServiceTierDefault
    deriving (Show, Eq, Ord, Generic)

$(deriveJsonEnum 11 ''ServiceTier)

data ReasoningEffort
    = ReasoningEffortNone
    | ReasoningEffortDefault
    | ReasoningEffortLow
    | ReasoningEffortMedium
    | ReasoningEffortHigh
    deriving (Show, Eq, Ord, Generic)

$(deriveJsonEnum 15 ''ReasoningEffort)

data ReasoningFormat
    = ReasoningFormatHidden
    | ReasoningFormatRaw
    | ReasoningFormatParsed
    deriving (Show, Eq, Ord, Generic)

$(deriveJsonEnum 5 ''ReasoningFormat)

data CitationOptions
    = CitationOptionsEnabled
    | CitationOptionsDisabled
    deriving (Show, Eq, Ord, Generic)

$(deriveJsonEnum 15 ''CitationOptions)

data ChatMessage = ChatMessage
    { role :: ChatRole
    , content :: Text
    , name :: Maybe Text
    }
    deriving (Show, Eq, Generic)

instance Default ChatMessage where
    def = ChatMessage
        { role = def
        , content = ""
        , name = Nothing
        }

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
    { messages :: Seq ChatMessage
    , model :: ModelId
    , citationOptions :: Maybe CitationOptions
    , compoundCustom :: Maybe Value
    -- ^ TODO
    , disableToolValidation :: Maybe Bool
    , documents :: Maybe [Value]
    -- ^ TODO
    , includeReasoning :: Maybe Bool
    , maxCompletionTokens :: Maybe Int
    , parallelToolCalls :: Maybe Bool
    , reasoningEffort :: Maybe ReasoningEffort
    -- ^ TODO only some models support this!
    -- qwen3 models support the following values Set to 'none' to disable reasoning. Set to 'default' or null to let Qwen reason.
    -- openai/gpt-oss-20b and openai/gpt-oss-120b support 'low', 'medium', or 'high'. 'medium' is the default value.
    , reasoningFormat :: Maybe ReasoningFormat
    -- ^ TODO mutually exclusive with reasoning effort.
    , responseFormat :: Maybe Value
    -- ^ TODO add the options here
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

$(deriveJSON (jsonOptions 0) ''ChatCreateRequest)

-- | Uses groq compound mini by default and includes no message
instance Default ChatCreateRequest where
    def =
        ChatCreateRequest
            { messages = mempty
            , model = def
            , citationOptions = Nothing
            , compoundCustom = Nothing
            , disableToolValidation = Nothing
            , documents = Nothing
            , includeReasoning = Nothing
            , maxCompletionTokens = Nothing
            , parallelToolCalls = Nothing
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

data ChatUsage = ChatUsage
    { queueTime :: Maybe Double
    , promptTokens :: Maybe Int
    , promptTime :: Maybe Double
    , completionTokens :: Maybe Int
    , completionTime :: Maybe Double
    , totalTokens :: Maybe Int
    , totalTime :: Maybe Double
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatUsage)

data ChatUsageBreakdown = ChatUsageBreakdown
    { model :: Maybe ModelId
    , usage :: Maybe ChatUsage
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatUsageBreakdown)

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
    , usageBreakdown :: Maybe ChatUsageBreakdown
    , systemFingerprint :: Maybe Text
    , serviceTier :: Maybe ServiceTier
    , xGroq :: Maybe ChatXGroq
    , mcpListTools :: Maybe [Value]
    }
    deriving (Show, Eq, Generic)

$(deriveJSON (jsonOptions 0) ''ChatCompletion)
