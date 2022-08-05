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

import           Parser.Descriptors.Types   (Descriptor (StrArr))
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

sinQuotes = between (symbol "'") (symbol "'")

doubQuotes = between (symbol "\"") (symbol "\"")

identifier' :: Parser String
identifier' = lexeme ((:) <$> (letterChar <|> PStr.char '_') <*> many (alphaNumChar <|> PStr.char '_'))

-- parse a Javascript identifier
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
jsVarDef = do
  try (symbol "let") <|> try (symbol "var") <|> symbol "const"
  lookAhead $ do
    identifier
    symbol "="
    symbol "new"
    schemaFun

-- mongoose.Schema or Schema
schemaFun :: Parser T.Text
schemaFun = try (symbol "mongoose.Schema") <|> symbol "Schema"

-- <type>
schemaType :: Parser T.Text
schemaType = do
  symbol "<"
  identifier
  symbol ">"

strLiteral :: Parser String
strLiteral = PStr.char '"' >> PLex.charLiteral `manyTill` PStr.char '"'

altStrLiteral :: Parser String
altStrLiteral = PStr.char '\'' >> PLex.charLiteral `manyTill` PStr.char '\''

-- parses ['a', 'b', 'c']
arrProps :: Parser Descriptor
arrProps = do
  val <- squareBrackets $ commaSep (try strLiteral <|> altStrLiteral)
  pure $ StrArr val

arrProps' :: Parser [String]
arrProps' = squareBrackets $ commaSep (try strLiteral <|> altStrLiteral)
