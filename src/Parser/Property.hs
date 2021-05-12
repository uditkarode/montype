{-# LANGUAGE OverloadedStrings #-}
module Parser.Property where

import           Data.List                (union)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Text.Parsec              as P (anyChar, anyToken, char,
                                                lookAhead, manyTill, oneOf,
                                                optionMaybe, try, (<|>))
import           Text.Parsec.String       as PStr (Parser)

import           Parser.Descriptor        (getDescriptor)
import           Parser.Descriptors.Types (Descriptor)
import           Parser.Miscellaneous     (commaSep, curly, identifier, s)

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
  s >> P.char ':' >> s
  Property (T.pack itName) <$> getDescriptor

-- parses a schema option (a: b)
schemaOption :: Parser (Text, Text)
schemaOption = do
  s
  key <- identifier
  s >> P.char ':' >> s
  val <- P.anyToken `P.manyTill` (s >> lookAhead (P.oneOf [',', '}']))
  s
  pure (T.pack key, T.pack val)

-- parses the second argument to a schema,
-- the object containing schema options
schemaOptions :: Parser [(Text, Text)]
schemaOptions = do
  s >> P.char ',' >> s
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
      "_id" -> if val == "true" then acc <> [ObjectProperty "_id" "Schema.Types.ObjectId"] else acc
      -- adds createdAt and updatedAt values to the schema
      "timestamps" -> acc <> [ObjectProperty "createdAt" "Date", ObjectProperty "updatedAt" "Date"]
      -- discard options that are unsupported / don't add anything to the schema
      _    -> acc

-- Parses the first and second argument to a schema, and
-- automatically adds required properties to the property
-- list so that they can be added to the final interface
schemaProperties :: Parser [Property]
schemaProperties = do
  properties <- curly $ commaSep (schemaProperty <|> emptyProperty)

  options <- optionMaybe (try schemaOptions)

  case options of
    Nothing -> pure properties
    Just options -> do
      pure $ union properties (getPropsFromOptions options)
