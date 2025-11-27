{-# LANGUAGE DeriveAnyClass #-}

module Groq.Config where

import Data.Aeson qualified as Aeson
import Data.Default
import Data.Text qualified as T
import GHC.Generics

import Groq.Models

data APIKey
  = {- | Load an api key from env variable
    The shell that this process is run in must have access.
    -}
    FromEnv String
  | -- | read an api key from the first line of a provided file
    FromFile FilePath
  | -- | Just directly pass an api key in
    APIKey String
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
  { model :: ModelId
  {- ^ control which model is used when doing inference
  some models have less/more features, or are better
  for certain purposes.
  Reference https://console.groq.com/docs/models for
  more information.
  -}
  , temperature :: Maybe Double
  -- ^ This controls how "random" a model's responses are
  , maxCompletionTokens :: Maybe Int
  -- ^ fine tune the max amount of tokens used per request.
  , systemPrompt :: Maybe T.Text
  {- ^ System prompts allow you to modify the tone
  or internal tasking of a llm.
  For example @Just "You respond only in french"@
  will limit the responses to french.
  -}
  , topP :: Maybe Double
  , apiKey :: APIKey
  {- ^ Control how the lib obtains an api key.
  Currently from file, from env var, or directly passed
  are the only options.
  -}
  }
  deriving (Generic)
  deriving anyclass (Aeson.FromJSON, Aeson.ToJSON)

instance Default GroqCfg where
  def =
    GroqCfg
      { model = Model_llama_3_1_8b_instant
      , temperature = Just 1
      , maxCompletionTokens = Just 1024
      , systemPrompt = Nothing
      , -- , stream = Just False
        topP = Just 1
      , apiKey = FromEnv "GROQ_API_KEY"
      }
