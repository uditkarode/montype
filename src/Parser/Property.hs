{-# LANGUAGE OverloadedStrings #-}
module Parser.Property where

import           Control.Applicative      (optional)
import           Data.List                (union)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.Megaparsec          as P (MonadParsec (lookAhead, try),
                                                anySingle, manyTill, oneOf,
                                                optional, (<|>))
import           Text.Megaparsec.Char     as PStr (char)

import           Parser.Descriptor        (getDescriptor)
import           Parser.Descriptors.Types (Descriptor)
import           Parser.Miscellaneous     (commaSep, curly, identifier, s)
import           Utils                    (Parser)

data Property
  = Property Text Descriptor
  | ObjectProperty Text Text
  | NoProperty
  deriving (Eq, Ord, Show)

-- an empty property, used to handle trailing commas
emptyProperty :: Parser Property
emptyProperty = do
  s
  pure NoProperty

-- an empty option, used to handle trailing commas
emptyOption :: Parser (Text, Text)
emptyOption = pure ("", "")

-- parses a schema property (a: b)
schemaProperty :: Parser Property
schemaProperty = do
  s
  itName <- identifier
  s >> PStr.char ':' >> s
  Property itName <$> getDescriptor

-- P.anyToken but accepts
-- spaces in between
anyTokenS :: Parser Char
anyTokenS = do
  v <- optional (try $ lookAhead P.anySingle)
  case v of
    Nothing -> do
      s
      pure ' '
    Just parsed -> P.anySingle

-- parses any text with spaces until a ',' or '}'
-- is encountered; the ',' or '}' is not consumed
rightSide :: Parser String
rightSide = anyTokenS `P.manyTill` (s >> lookAhead (P.oneOf [',', '}']))

-- parses a schema option (a: b)
schemaOption :: Parser (Text, Text)
schemaOption = do
  s
  key <- identifier
  s >> PStr.char ':' >> s
  val <- try (curly rightSide) <|> rightSide
  s
  pure (key, T.pack val)

-- parses the second argument to a schema,
-- the object containing schema options
schemaOptions :: Parser [(Text, Text)]
schemaOptions = do
  s >> PStr.char ',' >> s
  curly $ commaSep (schemaOption <|> emptyOption)

myFoldl :: Foldable t => t a -> b -> (b -> a -> b) -> b
myFoldl list def func = foldl func def list

-- gets properties that the given
-- option map can add to a schema
-- https://mongoosejs.com/docs/guide.html
getPropsFromOptions :: [(Text, Text)] -> [Property]
getPropsFromOptions options = do
  myFoldl options [] $ \acc curr -> let { key = fst curr; val = snd curr } in
    case key of
      -- adds a string casted object ID to the schema
      "id" -> if val == "true" then acc <> [ObjectProperty "id" "string"] else acc
      -- adds an object ID to the schema without string casting it
      "_id" -> if val == "true" then acc <> [ObjectProperty "_id" "mongoose.Schema.Types.ObjectId"] else acc
      -- adds createdAt and updatedAt values to the schema
      "timestamps" -> if val == "true" then acc <> [ObjectProperty "createdAt" "Date", ObjectProperty "updatedAt" "Date"] else acc
      -- discard options that are unsupported / don't add anything to the schema
      _    -> acc

-- Parses the first and second argument to a schema, and
-- automatically adds required properties to the property
-- list so that they can be added to the final interface
schemaProperties :: Parser [Property]
schemaProperties = do
  properties <- curly $ commaSep (schemaProperty <|> emptyProperty)

  options <- optional (try schemaOptions)

  case options of
    Nothing -> pure properties
    Just options -> do
      pure $ union properties (getPropsFromOptions options)
