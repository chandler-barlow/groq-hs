# groq-hs
groq cloud library for haskell


# Goals
The current goal of this project is to provide a nice chat/conversation interface for groq cloud.
Currently audio, responses, documents etc are out of scope. I will probably get to these at some point, but not today!

# Example

To run this library, the shell that executes this code needs access to `GROQ_API_KEY`

```haskell
{-# LANGUAGE OverloadedStrings #-}

import Data.Default
import Groq
import qualified Data.Text as T

main :: IO ()
main = do
  response <- execGroq def $ prompt "Say hello!"
  case response of
    Left (GroqError err) -> putStrLn $ "Failed due to " ++ err
    Right msg -> putStrLn $ "Got message " ++ T.unpack msg
```

All of the prompts are executed inside of `GroqT m` which will work with and `MonadIO m`.
The transformer exists to maintain Groq conversation state in between responses.
