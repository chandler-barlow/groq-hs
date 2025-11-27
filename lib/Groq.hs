{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Groq (
  prompt,
  execGroq,
  GroqCfg (..),
  GroqError (..),
  APIKey (..),
  GroqT, -- we dont need to export the internals
  updateDocuments,
  getDocuments,
  addDocument,
) where

import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (
  StateT (..),
  gets,
  modify,
 )
import Data.Generics.Labels ()
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Profunctor (Profunctor (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Lens.Micro
import Lens.Micro.Extras (view)

import Groq.ChatCompletion (
  Document,
  mkUserChatMessage,
 )
import Groq.Config
import Groq.Internal

{- |
  Used to run computations in the groq monad.
  Requires a config object to initialize chat state.

  @@
    {\- # LANGUAGE OverloadedStrings #-\}

    module Main where

    import Data.Default (def)
    import Groq (execGroq)

    main :: IO ()
    main = execGroq def $ pure ()
  @@
-}
execGroq :: (MonadIO m) => GroqCfg -> GroqT m a -> m (Either GroqError a)
execGroq cfg fn = runExceptT $ do
  ctx <- ExceptT $ initGroq cfg
  fmap fst
    . flip runStateT ctx
    $ runGroqT fn

{- |
  Send a prompt to the llm from inside of a groq computation

  @@
    {\- # LANGUAGE OverloadedStrings #-\}

    module Main where

    import Data.Default (def)
    import Groq (execGroq, prompt)

    main :: IO ()
    main = do
      response <- execGroq def $ prompt "Hello world!"
      case response of
        Left _ -> print "failure occurred"
        Right msg -> print msg
  @@

  You are free to prompt the llm as much as you like inside of the groq monad as well.
  This example spams "hello world!" to the llm.

  @@
    main :: IO ()
    main = void . execGroq def . forever $ do
        response <- prompt "Hello world!"
        case response of
          Left _ -> liftIO $ print "failure occurred"
          Right msg -> liftIO $ print msg
  @@
-}
prompt :: (MonadIO m) => Text -> GroqT m Text
prompt msg = do
  let
    request = mkUserChatMessage msg
  registerRequest request
  response <- makeRequest
  registerResponse response
  let
    rs = response ^. #choices
    rs' = view (#message . #content) <$> listToMaybe rs
  case rs' of
    Nothing -> throwError $ GroqError "Error: Absurd, no request was ever made"
    Just x -> pure x

{- |
  Groq cloud allows you to pass documents to the llm.
  It will be able to reference these when responding.
  The id of the document will be the name referenced in
  response.

  Documents can be either text, or json objects.
-}

-- | Update/adjust the set of documents available
updateDocuments ::
  (Monad m) =>
  (Set.Set Document -> Set.Set Document) ->
  GroqT m ()
updateDocuments =
  modify
    . over (#chatCtx . #documents)
    . dimap (fromMaybe Set.empty) Just

-- | List all documents that the model currently has access to
getDocuments :: (Monad m) => GroqT m (Set.Set Document)
getDocuments =
  gets $ fromMaybe Set.empty . view (#chatCtx . #documents)

-- | Insert a document into the set that the model has access to
addDocument :: (Monad m) => Document -> GroqT m ()
addDocument doc = updateDocuments $ Set.insert doc
