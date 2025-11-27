{-# LANGUAGE DeriveAnyClass #-}

module Groq.Config where

import Data.Aeson qualified as Aeson
import Data.Default
import Data.Text qualified as T
import GHC.Generics

import Groq.Models

newtype APIKey = FromEnv String
  deriving (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

{- |
  Default is ...

  @@
    GroqCfg
        { model = Model_llama_3_1_8b_instant
        , temperature = Just 1
        , maxCompletionTokens = Just 1024
        , topP = Just 1
        , apiKey = FromEnv "GROQ_API_KEY"
        }
  @@
-}
data GroqCfg = GroqCfg
  { model :: ModelId,
    temperature :: Maybe Double,
    maxCompletionTokens :: Maybe Int,
    {- | TODO streaming breaks everything because each frame
    comes tagged as data: ...
    and ends with data: [done]
    -}
    systemPrompt :: Maybe T.Text,
    -- , stream :: Maybe Bool

    topP :: Maybe Double,
    apiKey :: APIKey
  }
  deriving (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Default GroqCfg where
  def =
    GroqCfg
      { model = Model_llama_3_1_8b_instant,
        temperature = Just 1,
        maxCompletionTokens = Just 1024,
        systemPrompt = Nothing,
        -- , stream = Just False
        topP = Just 1,
        apiKey = FromEnv "GROQ_API_KEY"
      }
