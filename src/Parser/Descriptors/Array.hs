module Parser.Descriptors.Array where

import                          Text.Parsec                as P (getPosition,
                                                                 try, (<|>))
import                          Text.Parsec.Language       as PLan ()
import                          Text.Parsec.String         as PStr (Parser)

import                          Parser.Descriptors.Helpers (anyDescriptor,
                                                            shorthandDescriptor)
import {-# SOURCE #-}           Parser.Descriptors.Object  (objDescriptor)
import                          Parser.Descriptors.Types   (Descriptor (Array, Final, Shorthand),
                                                            SchemaArray (Nested, NoItems, SchemaArray),
                                                            TreeEndDescriptor (TreeEndDescriptor))
import                          Parser.Miscellaneous       (s, squareBrackets)

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
    try (anyDescriptor objDescriptor arrayDescriptor shorthandDescriptor) <|>
    try empty
  case res
        -- This array contains a Final type
        of
    Final x -> pure $ Array (SchemaArray x)
        -- This array contains another array
    Array y ->
      case y of
        NoItems -> pure $ Array (Nested NoItems)
        _       -> pure $ Array (Nested y)
        -- This array contains a Final type in the form of a Shorthand descriptor
    Shorthand z -> pure $ Array (SchemaArray (TreeEndDescriptor (z, [])))
    _ -> do
      pos <- getPosition
      error $
        "Element inside array must be a final property or another array! Found: " <>
        show res <> " at " <> show pos

-- Passes the contents of the
-- parsed array to arrayMaker
arrayDescriptor :: Parser Descriptor
arrayDescriptor = do
  squareBrackets arrayMaker
