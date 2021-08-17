{-# LANGUAGE OverloadedStrings #-}
module Parser.TopLevel where

import           Data.Text            (Text)
import           Text.Megaparsec      as P (Parsec, anySingle, manyTill, try)

import           Parser.Miscellaneous (identifier, jsVarDef, parens, s, s1,
                                       schemaFun, symbol, schemaType)
import           Parser.Property      (Property, schemaProperties)
import           Utils                (Parser)

-- takes: the entire schema
-- provides: tuple of variable name and list of properties
schema :: Parser (Text, [Property])
schema = do
  P.anySingle `P.manyTill` jsVarDef
  modelName <- identifier
  symbol "="
  symbol "new"
  schemaFun
  try schemaType
  modelContent <- parens schemaProperties
  pure (modelName, modelContent)
