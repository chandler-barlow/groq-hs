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

execGroq :: (MonadIO m) => GroqCfg -> GroqT m a -> m (Either GroqError a)
execGroq cfg fn = do
  ctx <- initGroq cfg
  fmap fst
    . flip runStateT ctx
    . runExceptT
    $ runGroqT fn

-- | Send a prompt to groq as a user
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

{- | helper function for editing the set of documents
that the model has access to
-}
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
