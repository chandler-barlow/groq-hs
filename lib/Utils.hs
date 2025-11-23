module Utils where

import Data.Aeson.TH
import Data.Char (toLower)
import Data.Aeson

--------------------------------------------------------------------------------
-- Helpers for Aeson deriving
--------------------------------------------------------------------------------

jsonOptions :: Int -> Options
jsonOptions prefixLen =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop prefixLen
    , omitNothingFields = True
    }

sumOptions :: Int -> Options
sumOptions prefixLen =
  defaultOptions
    { constructorTagModifier = map toLower . drop prefixLen
    }
