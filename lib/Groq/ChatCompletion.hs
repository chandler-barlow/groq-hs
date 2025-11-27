{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

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
  Document (..),
  DocumentSource (..),
  mkUserChatMessage,
  mkSysChatMessage,
) where

import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Default
import Data.Sequence (Seq)
import Data.Set (Set)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Groq.Internal.Utils
import Groq.Models

{- |

  This module exposes the types for interacting with the chat completion endpoint in
  groq cloud.

  # Warning:
  These types should really be considered internal. All of the functionality of
  this library is exposed in the top level @Groq@ module. Anything you are trying to do
  should be able to be done from their via some function or the main config. If you
  are needing something from here that is missing open a pr.

  This module is subject to change as the groq cloud api does.
-}
data ChatRole
  = ChatRoleUser
  | ChatRoleAssistant
  | ChatRoleSystem
  | ChatRoleTool
  | ChatRoleFunction
  deriving (Eq, Generic, Ord, Show)

instance Default ChatRole where
  def = ChatRoleUser

$(deriveJsonEnum 8 ''ChatRole)

data ServiceTier
  = ServiceTierAuto
  | ServiceTierOnDemand
  | ServiceTierFlex
  | ServiceTierPerformance
  | ServiceTierDefault
  deriving (Eq, Generic, Ord, Show)

$(deriveJsonEnum 11 ''ServiceTier)

data ReasoningEffort
  = ReasoningEffortNone
  | ReasoningEffortDefault
  | ReasoningEffortLow
  | ReasoningEffortMedium
  | ReasoningEffortHigh
  deriving (Eq, Generic, Ord, Show)

$(deriveJsonEnum 15 ''ReasoningEffort)

data ReasoningFormat
  = ReasoningFormatHidden
  | ReasoningFormatRaw
  | ReasoningFormatParsed
  deriving (Eq, Generic, Ord, Show)

$(deriveJsonEnum 5 ''ReasoningFormat)

data CitationOptions
  = CitationOptionsEnabled
  | CitationOptionsDisabled
  deriving (Eq, Generic, Ord, Show)

$(deriveJsonEnum 15 ''CitationOptions)

data ChatMessage = ChatMessage
  { role :: ChatRole
  , content :: Text
  , name :: Maybe Text
  }
  deriving (Eq, Generic, Show)

instance Default ChatMessage where
  def =
    ChatMessage
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

data DocumentSource
  = DocumentText T.Text
  | DocumentJSON Value
  deriving (Eq, Ord, Show)

instance FromJSON DocumentSource where
  parseJSON (Object x) = do
    ty <- (x .: "type" :: Parser Text)
    if ty == "text"
      then fmap DocumentText $ x .: "text"
      else fmap DocumentJSON $ x .: "data"
  parseJSON invalid =
    prependFailure
      "parsing DocumentSource failure "
      (typeMismatch "Object" invalid)

instance ToJSON DocumentSource where
  toJSON =
    \case
      DocumentText t ->
        object
          [ "type" .= String "text"
          , "text" .= String t
          ]
      DocumentJSON j ->
        object
          [ "type" .= String "json"
          , "data" .= j
          ]

data Document = Document
  { id :: Maybe String
  , source :: DocumentSource
  }
  deriving (Eq, Generic, Ord, Show)

$(deriveJSON (jsonOptions 0) ''Document)

data ResponseFormat
  = ResponseText
  | ResponseJSON
  | ResponseJSONSchema Value
  deriving (Eq, Show)

instance FromJSON ResponseFormat where
  parseJSON (Object x) = do
    ty <- (x .: "type" :: Parser Text)
    case ty of
      "text" -> pure ResponseText
      "json_object" -> pure ResponseJSON
      "json_schema" -> fmap ResponseJSONSchema $ x .: "json_schema"
      _ -> parseFail "Invalid type"
  parseJSON invalid =
    prependFailure
      "parsing DocumentSource failure "
      (typeMismatch "Object" invalid)

instance ToJSON ResponseFormat where
  toJSON =
    \case
      ResponseText -> object ["type" .= String "text"]
      ResponseJSON -> object ["type" .= String "json_object"]
      ResponseJSONSchema s ->
        object
          [ "type" .= String "json_schema"
          , "json_schema" .= s
          ]

data SearchSettings = SearchSettings
  { country :: Maybe String
  , excludeDomains :: Maybe [String]
  , includeDomains :: Maybe [String]
  , includeImages :: Maybe Bool
  }
  deriving (Generic, Show, Eq)

$(deriveJSON (jsonOptions 0) ''SearchSettings)

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
  -- ^ TODO
  , -- , compoundCustom :: Maybe Value

    disableToolValidation :: Maybe Bool
  , documents :: Maybe (Set Document)
  -- ^ TODO
  , includeReasoning :: Maybe Bool
  , maxCompletionTokens :: Maybe Int
  , parallelToolCalls :: Maybe Bool
  , reasoningEffort :: Maybe ReasoningEffort
  {- ^ TODO only some models support this!
    qwen3 models support the following values Set to 'none' to disable reasoning. Set to 'default' or null to let Qwen reason.
    openai/gpt-oss-20b and openai/gpt-oss-120b support 'low', 'medium', or 'high'. 'medium' is the default value.
  -}
  , reasoningFormat :: Maybe ReasoningFormat
  -- ^ TODO mutually exclusive with reasoning effort.
  , responseFormat :: Maybe ResponseFormat
  -- ^ TODO add the options here
  , searchSettings :: Maybe SearchSettings
  -- ^ TODO
  , seed :: Maybe Int
  , serviceTier :: Maybe ServiceTier
  , stop :: Maybe [String]
  , store :: Maybe Bool
  , -- , stream :: Maybe Bool
    -- , streamOptions :: Maybe Value
    -- -- ^ TODO
    temperature :: Maybe Double
  , -- , toolChoice :: Maybe Value
    -- -- ^ TODO: These are high value
    -- , tools :: Maybe [Value]
    -- -- ^ TODO: These are high value
    topLogprobs :: Maybe Int
  , topP :: Maybe Double
  , user :: Maybe Text
  }
  deriving (Generic, Show, Eq)

$(deriveJSON (jsonOptions 0) ''ChatCreateRequest)

-- | Uses groq compound mini by default and includes no message
instance Default ChatCreateRequest where
  def =
    ChatCreateRequest
      { messages = mempty
      , model = def
      , citationOptions = Nothing
      , -- , compoundCustom = Nothing
        disableToolValidation = Nothing
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
      , -- , stream = Nothing
        -- , streamOptions = Nothing
        temperature = Nothing
      , -- , toolChoice = Nothing
        -- , tools = Nothing
        topLogprobs = Nothing
      , topP = Nothing
      , user = Nothing
      }

data ChatChoice = ChatChoice
  { index :: Int
  , message :: ChatMessage
  , logprobs :: Maybe Value
  , finishReason :: Maybe Text
  }
  deriving (Eq, Generic, Show)

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
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOptions 0) ''ChatUsage)

data ChatUsageBreakdown = ChatUsageBreakdown
  { model :: Maybe ModelId
  , usage :: Maybe ChatUsage
  }
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOptions 0) ''ChatUsageBreakdown)

newtype ChatXGroq = ChatXGroq
  { cxId :: Maybe Text
  }
  deriving (Eq, Generic, Show)

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
  deriving (Eq, Generic, Show)

$(deriveJSON (jsonOptions 0) ''ChatCompletion)
