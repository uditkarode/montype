{-# LANGUAGE OverloadedStrings #-}

module Codegen.Getters where

import                          Data.Either.Combinators
import                          Data.List                (find)
import                qualified Data.Map                 as M
import                          Data.Maybe               (fromJust, isJust)
import                          Data.Text                as T (Text, pack,
                                                               replicate,
                                                               unpack)
import                          Text.Megaparsec          as P (parse)

import {-# SOURCE #-}           Codegen.Helpers          (getTsType, myFoldM,
                                                          tab)
import                          Config                   (getMapped)
import                          Parser.Descriptors.Types (Descriptor (NoValue),
                                                          SchemaArray (..),
                                                          TreeEndDescriptor (..))
import                          Parser.Miscellaneous     (arrProps, arrProps')


myFoldl :: Foldable t => t a -> b -> (b -> a -> b) -> b
myFoldl list def func = foldl func def list

-- TreeEndDescriptor -> TS
getFinal :: TreeEndDescriptor -> M.Map Text Text -> Either String Text
getFinal (TreeEndDescriptor ("Map", props)) userTypes = do
  let ofType = (fmap snd . find ((== "of") . fst)) props
  if isJust ofType then do
    Right $ "Map<string, " <> fromJust ofType <> ">"
  else do
    let propName = "mongoose.Schema.Types.Map"
    Right $ fromRight propName (getMapped propName userTypes)

getFinal (TreeEndDescriptor ("String", props)) userTypes = do
  let enumArrStr = (fmap snd . find ((== "enum") . fst)) props
  if isJust enumArrStr then do
    -- if string contains enum validator, use it to generate the type instead
      let values = P.parse arrProps' "" (fromJust enumArrStr)
      if isLeft values then Left ("invalid enum array found: " <> T.unpack (fromJust enumArrStr))
      else do
        let vals = fromRight' values
        let tsType = myFoldl vals "" $ \acc curr -> acc <> "'" <> curr <> "' | "
        Right $ T.pack (init $ init $ init tsType)
  else
    -- if string doesn't contain enum validator, just map it as always
    getMapped "String" userTypes

getFinal (TreeEndDescriptor (t, _)) userTypes = getMapped t userTypes

getFinal (Literal l) userTypes = do
  -- allow user to overwrite literal types
  let mapped = getMapped l userTypes
  if isLeft mapped then Right l else Right (fromRight' mapped)

-- SchemaArray -> TS
getSchemaArrStr :: SchemaArray -> M.Map Text Text -> Either String Text
getSchemaArrStr (Nested x) userTypes = getSchemaArrStr x userTypes >>= \v -> pure $ v <> "[]"

getSchemaArrStr (SchemaArray t) userTypes = getFinal t userTypes >>= \v -> pure $ v <> "[]"

getSchemaArrStr NoItems userTypes         = Right "never"

-- IntermediateObject -> TS
getObjStr :: [(Text, Descriptor)] -> Int -> M.Map Text Text -> Either String Text
getObjStr props indent userTypes = objs >>= \v -> pure $ start <> v <> end
  where
    start = "{\n"
    objs =
      myFoldM props "" $ \acc curr -> do
        case snd curr of
          NoValue -> Right acc
          _ -> getTsType (snd curr) (fst curr) (indent + 1) userTypes >>= \v -> pure $ acc <> tab <> v <> ";\n"
    end = T.replicate indent tab <> tab <> "}"
