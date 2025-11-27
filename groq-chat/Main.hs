{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad (forever)
import qualified Control.Monad.Error.Class as Error
import Control.Monad.IO.Class (MonadIO (..))
import qualified Data.Aeson as Aeson
import Data.Default
import qualified Data.Text as T
import Options.Generic

import qualified Groq

newtype Options w = Options
  { config :: w ::: Maybe FilePath <?> "Location of config file"
  }
  deriving (Generic)

instance ParseRecord (Options Wrapped)

deriving instance Show (Options Unwrapped)

main :: IO ()
main = do
  opts <- unwrapRecord "Groq chat options" :: IO (Options Unwrapped)
  runApp opts

groqChat :: (MonadIO m) => Groq.GroqCfg -> m (Either Groq.GroqError ())
groqChat cfg = Groq.execGroq cfg $ do
  let
    puts = liftIO . putStrLn
  puts $
    unlines
      [ "+-----------------------------------------------------+",
        "| Welcome to a groq-chat!                             |",
        "| A sample cli for testing the haskell lib `groq-hs`  |",
        "| Make sure that you have the var GROQ_API_KEY set,   |",
        "| otherwise this application will not work...         |",
        "+-----------------------------------------------------+"
      ]
  forever $ do
    puts "[USER]"
    msg <- liftIO getLine
    res <- Error.tryError . Groq.prompt $ T.pack msg
    case res of
      Right res' -> do
        puts "[LLM]"
        puts $ T.unpack res'
      Left err -> do
        puts "Error: Failed to send prompt upstream."
        Error.throwError err

runApp :: Options Unwrapped -> IO ()
runApp opts = do
  let
    dumpError =
      \case
        Left e -> error $ show e
        Right _ -> pure ()
  case opts.config of
    Nothing -> groqChat def >>= dumpError
    Just path -> do
      mCfg <- Aeson.decodeFileStrict @Groq.GroqCfg path
      case mCfg of
        Nothing -> error "Error: Unable to parse the provided config file"
        Just cfg -> groqChat cfg >>= dumpError
