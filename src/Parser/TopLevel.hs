{-# LANGUAGE OverloadedStrings #-}
module Parser.TopLevel where

import           Data.Text            (Text)

import qualified Data.Text            as T
import           Data.Void
import           Text.Megaparsec      as P
import           Text.Megaparsec.Char as PStr

import           Parser.Miscellaneous (identifier, jsVarDef, parens, s, s1,
                                       schemaFun, symbol)
import           Parser.Property      (Property, schemaProperties)

type Parser = Parsec Void T.Text

-- takes: the entire schema
-- provides: tuple of variable name and list of properties
schema :: Parser (Text, [Property])
schema = do
  P.anySingle `P.manyTill` jsVarDef
  modelName <- identifier
  symbol "="
  symbol "new"
  schemaFun
  modelContent <- parens schemaProperties
  pure (modelName, modelContent)
