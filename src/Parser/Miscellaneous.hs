module Parser.Miscellaneous where

import           Text.Parsec          as P (anyChar, char, lookAhead, manyTill,
                                            newline, skipMany, skipMany1, space,
                                            string, try, (<|>))
import           Text.Parsec.Language as PLan (emptyDef)
import           Text.Parsec.String   as PStr (Parser)
import qualified Text.Parsec.Token    as PTok

langParser =
  PTok.makeTokenParser
    emptyDef
      { PTok.commentLine = "//"
      , PTok.commentStart = "/*"
      , PTok.commentEnd = "*/"
      , PTok.nestedComments = True
      }

curly = PTok.braces langParser

parens = PTok.parens langParser

commaSep = PTok.commaSep1 langParser

identifier = PTok.identifier langParser

squareBrackets = PTok.brackets langParser

-- Zero or more spaces or newlines
s :: Parser ()
s = P.skipMany P.space <|> P.skipMany P.newline

-- One or more spaces or newlines
s1 :: Parser ()
s1 = P.skipMany1 P.space <|> P.skipMany1 P.newline

-- var, let, or const
jsVarDef :: Parser String
jsVarDef = try (P.string "var") <|> try (P.string "let") <|> try (P.string "const")

-- mongoose.Schema or Schema
schemaFun :: Parser String
schemaFun = P.string "mongoose.Schema" <|> P.string "Schema"

-- String within two given characters
strWithin :: Char -> Char -> Parser String
strWithin a b = do
  P.char a >> s
  P.anyChar `P.manyTill` lookAhead (P.char b)
