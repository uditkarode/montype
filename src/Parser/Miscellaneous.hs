{-# LANGUAGE OverloadedStrings #-}
module Parser.Miscellaneous where

import           Data.Functor               ((<&>))
import qualified Data.Text                  as T
import           Text.Megaparsec            as P (MonadParsec (lookAhead, try),
                                                  anySingle, between, many,
                                                  manyTill, sepBy, skipMany,
                                                  skipSome, (<|>))
import           Text.Megaparsec.Char       as PStr (alphaNumChar, char,
                                                     letterChar, newline, space,
                                                     space1)
import qualified Text.Megaparsec.Char.Lexer as PLex
import           Utils                      (Parser)

sc :: Parser ()
sc = PLex.space
  space1
  (PLex.skipLineComment "//")
  (PLex.skipBlockComment "/*" "*/")

symbol = PLex.symbol sc

curly = between (symbol "{") (symbol "}")

parens = between (symbol "(") (symbol ")")

squareBrackets = between (symbol "[") (symbol "]")

commaSep p = P.sepBy p (symbol ",")

lexeme = PLex.lexeme sc

identifier' :: Parser String
identifier' = lexeme ((:) <$> letterChar <*> many alphaNumChar)

identifier :: Parser T.Text
identifier = identifier' <&> T.pack

-- Zero or more spaces or newlines
s :: Parser ()
s = PStr.space <|> P.skipMany PStr.newline

-- One or more spaces or newlines
s1 :: Parser ()
s1 = PStr.space <|> P.skipSome PStr.newline

-- var, let, or const
jsVarDef :: Parser T.Text
jsVarDef = try (symbol "let") <|> try (symbol "var") <|> symbol "const"

-- mongoose.Schema or Schema
schemaFun :: Parser T.Text
schemaFun = try (symbol "mongoose.Schema") <|> symbol "Schema"

-- String within two given characters
strWithin :: Char -> Char -> Parser String
strWithin a b = do
  PStr.char a >> s
  P.anySingle `P.manyTill` lookAhead (PStr.char b)
