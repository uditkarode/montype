{-# LANGUAGE OverloadedStrings #-}
module Parser.Descriptors.Object where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Parsec                as P (char, (<|>))
import           Text.Parsec.String         as PStr (Parser)
import qualified Text.Parsec.Token          as PTok

import           Parser.Descriptors.Array   (arrayDescriptor)
import           Parser.Descriptors.Helpers (anyDescriptor, justDescriptor,
                                             myMap, search)
import           Parser.Descriptors.Types   (Descriptor (Descriptor, Final, IntermediateObject, NoValue),
                                             TreeEndDescriptor (TreeEndDescriptor))
import           Parser.Miscellaneous       (commaSep, curly, identifier, s)

-- used to implement trailing comma
emptyObj :: Parser (Text, Descriptor)
emptyObj = pure ("", NoValue)

-- parses a: (descriptor)
propertyDesc :: Parser (Text, Descriptor)
propertyDesc = do
  s
  key <- identifier
  s >> P.char ':' >> s
  val <- anyDescriptor objDescriptor arrayDescriptor justDescriptor
  s
  pure (T.pack key, val)

-- Contains functions for when the right side is an object
-- parses { propertyDesc, propertyDesc, ... }
propertiesDesc :: Parser [(Text, Descriptor)]
propertiesDesc = do
  curly $ commaSep (propertyDesc <|> emptyObj)

-- only used when a TreeEndDescriptor is encountered
-- converts a [(String, Descriptor)] to [(String, String)]
-- only pattern matches Descriptor because a TreeEndDescriptor
-- can not have further nesting
makeFinal :: [(Text, Descriptor)] -> [(Text, Text)]
makeFinal val = do
  myMap val $ \x ->
    case snd x of
      Descriptor d ->
        if fst x == "type"
          then Nothing
          else Just (fst x, d)

-- parses an object and returns an IntermediateObject
-- or a Final TreeEndDescriptor, where an IntermediateObject
-- is a nested object while a Final TreeEndDescriptor is an object
-- that contains the `type` descriptor
objDescriptor :: Parser Descriptor
objDescriptor = do
  val <- propertiesDesc
  let isFinal = search "type" val
  case isFinal of
    Nothing -> pure $ IntermediateObject val
    Just x -> do
      case x of
        Descriptor t -> pure (Final (TreeEndDescriptor (t, makeFinal val)))
        Final t      -> pure $ Final t
