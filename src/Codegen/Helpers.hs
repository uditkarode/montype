{-# LANGUAGE OverloadedStrings #-}
module Codegen.Helpers where

import           Codegen.Getters          (getFinal, getObjStr, getSchemaArrStr)
import           Config                   (getMapped)
import           Control.Monad            (foldM)
import qualified Data.Map                 as M
import           Data.Text                (Text)
import qualified Data.Text                as T

import           Parser.Descriptors.Types (Descriptor (Array, Descriptor, Final, IntermediateObject))

tab :: T.Text
tab = "  "

-- foldM but with the function coming later
myFoldM :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
myFoldM list def func = foldM func def list

-- get TS type from Descriptor
getTsType :: Descriptor -> Text -> Int -> M.Map Text Text -> Either String Text
getTsType (Descriptor desc) name indent userTypes = getMapped desc userTypes >>= \v -> pure $ T.replicate indent tab <> name <> ": " <> v
getTsType (Array arr) name indent userTypes = getSchemaArrStr arr userTypes >>= \v -> pure $ T.replicate indent tab <> name <> ": " <> v
getTsType (IntermediateObject obj) name indent userTypes = getObjStr obj indent userTypes >>= \v -> pure $ T.replicate indent tab <> name <> ": " <> v
getTsType (Final t) name indent userTypes = getFinal t userTypes >>= \v -> pure $ T.replicate indent tab <> name <> ": " <> v

-- get TS type from Descriptor (for root call)
getTsType' :: Descriptor -> Int -> M.Map Text Text -> Either String Text
getTsType' (Descriptor desc) indent userTypes = getMapped desc userTypes
getTsType' (Array arr) indent userTypes = getSchemaArrStr arr userTypes
getTsType' (IntermediateObject obj) indent userTypes = getObjStr obj indent userTypes
getTsType' (Final t) indent userTypes = getFinal t userTypes
