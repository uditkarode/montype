module Parser.TopLevel where

import           Data.Text            (Text)

import qualified Data.Text            as T
import           Text.Parsec          as P (anyChar, anyToken, char, manyTill,
                                            string)
import           Text.Parsec.String   as PStr (Parser)
import qualified Text.Parsec.Token    as PTok

import           Parser.Miscellaneous (identifier, jsVarDef, parens, s, s1,
                                       schemaFun)
import           Parser.Property      (Property, schemaProperties)


-- takes: the entire schema
-- provides: tuple of variable name and list of properties
schema :: Parser (Text, [Property])
schema = do
  P.anyToken `P.manyTill` jsVarDef >> s1
  modelName <- identifier
  s >> P.char '=' >> s
  P.string "new" >> s1
  schemaFun >> s
  modelContent <- parens schemaProperties
  pure (T.pack modelName, modelContent)
