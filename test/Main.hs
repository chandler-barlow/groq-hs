{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.FileEmbed (embedFile)
import Test.Tasty
import Test.Tasty.HUnit

import Groq.ChatCompletion qualified as Groq

main :: IO ()
main = defaultMain tests

json :: BSL.ByteString -> Maybe Aeson.Value
json = Aeson.decode @Aeson.Value

tests :: TestTree
tests =
  testGroup
    "groq-hs tests"
    [ golden
    ]

roundTripJSON ::
  forall a.
  (Aeson.FromJSON a, Aeson.ToJSON a) =>
  BS.ByteString ->
  Assertion
roundTripJSON bs =
  let
    mx = Aeson.eitherDecode @a $ BS.fromStrict bs
  in
    case mx of
      Left e -> assertFailure $ "Failed to parse input bytes into value with error: " ++ e
      Right x ->
        assertEqual
          "encoding matches the input"
          (json $ Aeson.encode x)
          (json $ BS.fromStrict bs)

goldenChatRequest :: TestTree
goldenChatRequest =
  testCase "Chat completion request roundtrips" $
    let
      goldenRequest = $(embedFile "./test/golden/chat_request.json")
    in
      roundTripJSON @Groq.ChatCreateRequest (BS.init goldenRequest)

goldenChatResponse :: TestTree
goldenChatResponse =
  testCase "Chat completion request roundtrips" $
    let
      goldenResponse = $(embedFile "./test/golden/chat_response.json")
    in
      roundTripJSON @Groq.ChatCompletion (BS.init goldenResponse)

golden :: TestTree
golden =
  testGroup
    "All aeson instances round trip golden copies"
    [ goldenChatRequest
    , goldenChatResponse
    ]
