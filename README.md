# *WARNING* I am not planning on maintaining this further, when I initially created this I didn't realize that there are already very good openai bindings for haskell :)
# groq-hs
groq cloud library for haskell

# TODO
- add tools ( for agents )

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

Additionally there is an example application called `groq-chat` that can be run via `nix run .#groq-chat`. This demo program let's you talk to some llm on groq cloud using a groq api key.
