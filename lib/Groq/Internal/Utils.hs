{-# LANGUAGE TemplateHaskell #-}

module Groq.Internal.Utils where

import Data.Aeson
import Data.Char (toLower)
import Data.Text qualified as T
import Language.Haskell.TH
import Text.Casing

jsonOptions :: Int -> Options
jsonOptions prefixLen =
  defaultOptions
    { fieldLabelModifier = camelTo2 '_' . drop prefixLen,
      omitNothingFields = True
    }

sumOptions :: Int -> Options
sumOptions prefixLen =
  defaultOptions
    { constructorTagModifier = map toLower . drop prefixLen
    }

{- | Generate ToJSON + FromJSON for a sum type.
Only used to skip some boiler plate with groq enums
-}
deriveJsonEnum :: Int -> Name -> Q [Dec]
deriveJsonEnum n typeName = do
  TyConI dec <- reify typeName
  cons <- case dec of
    DataD _ _ _ _ cs _ -> pure cs
    NewtypeD {} ->
      fail "deriveJsonEnum: not for newtypes"
    _ ->
      fail "deriveJsonEnum: expected a data type"

  let
    mkNamePair con =
      let
        conNameStr = nameBase $ case con of
          NormalC n' _ -> n'
          RecC n' _ -> n'
          InfixC _ n' _ -> n'
          ForallC _ _ c -> case c of
            NormalC n' _ -> n'
            RecC n' _ -> n'
            InfixC _ n' _ -> n'
            _ -> error "unsupported constructor"
          _ -> error "Unsupported constructor"

        -- We want something like $(deriveJsonEnum ''MyUserName) to be "user_name"
        jsonStr = toQuietSnake . fromHumps $ drop n conNameStr
      in
        (conNameStr, jsonStr)

  let
    mapping = map mkNamePair cons

  toJsonInst <- mkToJson typeName mapping
  fromJsonInst <- mkFromJson typeName mapping

  pure [toJsonInst, fromJsonInst]

mkToJson :: Name -> [(String, String)] -> Q Dec
mkToJson typeName mapping = do
  x <- newName "x"
  let
    matches =
      [ match (conP (mkName hsName) []) (normalB (body jsonTxt)) []
      | (hsName, jsonTxt) <- mapping
      ]
    body s = [|toJSON (T.pack s)|]
  fun <- funD 'toJSON [clause [varP x] (normalB (caseE (varE x) matches)) []]
  pure $ InstanceD Nothing [] (AppT (ConT ''ToJSON) (ConT typeName)) [fun]

mkFromJson :: Name -> [(String, String)] -> Q Dec
mkFromJson typeName mapping = do
  v <- newName "v"
  let
    mkMatch (hsName, jsonTxt) =
      match
        (litP (StringL jsonTxt))
        (normalB [|pure $(conE (mkName hsName))|])
        []
    fallback =
      match
        wildP
        (normalB [|fail ("Invalid JSON enum for " ++ $(litE (StringL (nameBase typeName))))|])
        []

  let
    parseCase = do
      x <- newName "x"
      [|
        do
          $(varP x) <- T.unpack <$> parseJSON $(varE v)
          $(caseE (varE x) (map mkMatch mapping ++ [fallback]))
        |]

  fun <-
    funD
      'parseJSON
      [ clause
          [varP v]
          (normalB parseCase)
          []
      ]

  pure $ InstanceD Nothing [] (AppT (ConT ''FromJSON) (ConT typeName)) [fun]
