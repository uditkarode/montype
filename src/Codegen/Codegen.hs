{-# LANGUAGE OverloadedStrings #-}

module Codegen.Codegen where

import           Control.Monad   (foldM)
import qualified Data.Map        as M
import           Data.Text       as T (Text)
import Data.Either ( fromRight )

import           Codegen.Helpers (getTsType', myFoldM, tab)
import           Parser.Property (Property (ObjectProperty, Property))
import Config ( getMapped )

-- takes a schema name and a list of properties,
-- and returns the TypeScript interface for it
makeInterface :: Text -> [Property] -> M.Map Text Text -> Either String Text
makeInterface name properties userTypes = props >>= \i -> pure $ start <> i <> end
  where
    start = "export interface " <> name <> " {\n"
    props =
      myFoldM properties "" $ \acc curr -> do
        case curr of
          Property name descriptor -> do
            let tsDescrip = getTsType' descriptor 0 userTypes
            tsDescrip >>= \descrip -> pure $ acc <> tab <> name <> ": " <> descrip <> ";\n"
          ObjectProperty name tsType -> Right $ acc <> tab <> name <> ": " <> fromRight tsType (getMapped tsType userTypes) <> ";\n"
          _ -> Right acc
    end = "};\n"
