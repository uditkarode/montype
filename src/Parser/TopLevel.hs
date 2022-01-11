{-# LANGUAGE OverloadedStrings #-}

module Parser.TopLevel where

import Data.Text (Text)
import Parser.Miscellaneous
  ( identifier,
    jsVarDef,
    parens,
    s,
    s1,
    schemaFun,
    schemaType,
    symbol,
  )
import Parser.Property (Property, schemaProperties)
import Text.Megaparsec as P (Parsec, anySingle, manyTill, optional, try)
import Utils (Parser)

-- takes: the entire schema
-- provides: tuple of variable name and list of properties
schema :: Parser (Text, [Property])
schema = do
  P.anySingle `P.manyTill` jsVarDef
  modelName <- identifier
  symbol "="
  symbol "new"
  schemaFun
  optional schemaType
  modelContent <- parens schemaProperties
  pure (modelName, modelContent)
