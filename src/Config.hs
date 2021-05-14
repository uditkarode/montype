{-# LANGUAGE OverloadedStrings #-}
module Config where

import           Data.Map  as M (Map, fromList, lookup)
import           Data.Text (Text)
import qualified Data.Text as T

-- get the value that an AST type maps to.
-- error out if no value can be found, since
-- generating faulty interfaces is not a choice
getMapped :: Text -> M.Map Text Text -> Either String Text
getMapped astType userTypes = do
  case M.lookup astType userTypes of
    Nothing ->
      case M.lookup astType typeMap of
        Nothing ->
          Left ("couldn't map the schema type '" <> T.unpack astType <> "' to a TypeScript type!")
        Just y -> Right y
    Just x -> Right x

-- default map from schema types to TS types
typeMap :: Map Text Text
typeMap =
  fromList
    [ ("String", "string")
    , ("Number", "number")
    , ("Date", "Date")
    , ("Buffer", "mongoose.Schema.Types.Buffer")
    , ("Boolean", "boolean")
    , ("Mixed", "mongoose.Schema.Types.Mixed")
    , ("Decimal128", "mongoose.Schema.Types.Decimal128")
    , ("ObjectId", "mongoose.Schema.Types.ObjectId")
    ]
