{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Default
import Groq.ChatCompletion qualified as Groq
import Groq.Config qualified as Groq
import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

json :: BSL.ByteString -> Maybe Aeson.Value
json = Aeson.decode @Aeson.Value

tests :: TestTree
tests =
  testGroup
    "groq-hs tests"
    [ roundTrips
    ]

roundTripJSON :: (Show a, Eq a, Aeson.FromJSON a, Aeson.ToJSON a) => a -> Assertion
roundTripJSON v =
  assertEqual "json instance round trips" (Aeson.decode $ Aeson.encode v) (Just v)

roundTripChatRequest :: TestTree
roundTripChatRequest =
  testCase "Chat completion request round trips" $ roundTripJSON @Groq.ChatCreateRequest def

-- roundTripChatResponse :: TestTree
-- roundTripChatResponse =
--   testCase "Chat completion response round trips" $ roundTripJSON @Groq.ChatCompletion def

roundTripConfig :: TestTree
roundTripConfig =
  testCase "Config round trips" $ roundTripJSON @Groq.GroqCfg def

roundTrips :: TestTree
roundTrips =
  testGroup
    "All aeson instances round trip"
    [ roundTripChatRequest
    -- , roundTripChatResponse
    , roundTripConfig
    ]
