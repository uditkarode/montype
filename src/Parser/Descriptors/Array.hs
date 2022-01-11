module Parser.Descriptors.Array where

import qualified Data.Text as T
import Data.Void
import Parser.Descriptors.Helpers
  ( anyDescriptor,
    shorthandDescriptor,
  )
import {-# SOURCE #-} Parser.Descriptors.Object (objDescriptor)
import Parser.Descriptors.Types
  ( Descriptor (Array, Final, Shorthand),
    SchemaArray (Nested, NoItems, SchemaArray),
    TreeEndDescriptor (TreeEndDescriptor),
  )
import Parser.Miscellaneous (s, squareBrackets)
import Text.Megaparsec as P
  ( Parsec,
    getSourcePos,
    try,
    (<|>),
  )

type Parser = Parsec Void T.Text

-- parses whitespace or nothing
-- used to parse an empty array
empty :: Parser Descriptor
empty = do
  s
  pure $ Array NoItems

-- takes the contents of an array
-- and returns an Array Descriptor
arrayMaker :: Parser Descriptor
arrayMaker = do
  res <-
    try (anyDescriptor objDescriptor arrayDescriptor shorthandDescriptor)
      <|> try empty
  case res of
    -- This array contains a Final type

    Final x -> pure $ Array (SchemaArray x)
    -- This array contains another array
    Array y ->
      case y of
        NoItems -> pure $ Array (Nested NoItems)
        _ -> pure $ Array (Nested y)
    -- This array contains a Final type in the form of a Shorthand descriptor
    Shorthand z -> pure $ Array (SchemaArray (TreeEndDescriptor (z, [])))
    _ -> do
      pos <- getSourcePos
      error $
        "Element inside array must be a final property or another array! Found: "
          <> show res
          <> " at "
          <> show pos

-- Passes the contents of the
-- parsed array to arrayMaker
arrayDescriptor :: Parser Descriptor
arrayDescriptor = do
  squareBrackets arrayMaker
