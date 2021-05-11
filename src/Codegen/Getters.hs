{-# LANGUAGE OverloadedStrings #-}

module Codegen.Getters where

import                          Data.List                (find)
import                qualified Data.Map                 as M
import                          Data.Maybe               (fromJust, isJust)
import                          Data.Text                as T (Text, replicate)

import {-# SOURCE #-}           Codegen.Helpers          (getTsType, myFoldM,
                                                          tab)
import                          Config                   (getMapped)
import                          Parser.Descriptors.Types (Descriptor (NoValue),
                                                          SchemaArray (..),
                                                          TreeEndDescriptor (..))

-- TreeEndDescriptor -> TS
getFinal :: TreeEndDescriptor -> M.Map Text Text -> Either String Text
getFinal (TreeEndDescriptor ("Map", props)) userTypes = do
  let ofType = (fmap snd . find ((== "of") . fst)) props
  if isJust ofType then do
    Right $ "Map<string, " <> fromJust ofType <> ">"
  else
    Right "mongoose.Schema.Types.Map"

getFinal (TreeEndDescriptor (t, _)) userTypes     = getMapped t userTypes
getFinal (Literal t) userTypes                    = Right t

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
