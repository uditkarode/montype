{-# LANGUAGE OverloadedStrings #-}
module Parser.Descriptors.Object where

import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Text.Megaparsec            as P ((<|>))
import           Text.Megaparsec.Char       as PStr (char)

import           Parser.Descriptors.Array   (arrayDescriptor)
import           Parser.Descriptors.Helpers (anyDescriptor, anyDescriptor',
                                             justDescriptor, myMap, search)
import           Parser.Descriptors.Types   (Descriptor (Descriptor, Final, IntermediateObject, NoValue, StrArr),
                                             TreeEndDescriptor (TreeEndDescriptor))
import           Parser.Miscellaneous       (arrProps, commaSep, curly,
                                             identifier, s)
import           Utils                      (Parser)

-- used to implement trailing comma
emptyObj :: Parser (Text, Descriptor)
emptyObj = pure ("", NoValue)

-- parses a: (descriptor)
propertyDesc :: Parser (Text, Descriptor)
propertyDesc = do
  s
  key <- identifier
  s >> PStr.char ':' >> s
  val <- anyDescriptor' objDescriptor arrayDescriptor arrProps justDescriptor
  s
  pure (key, val)

-- Contains functions for when the right side is an object
-- parses { propertyDesc, propertyDesc, ... }
propertiesDesc :: Parser [(Text, Descriptor)]
propertiesDesc = do
  curly $ commaSep (propertyDesc <|> emptyObj)

-- only used when a TreeEndDescriptor is encountered
-- converts a [(String, Descriptor)] to [(String, String)]
-- only pattern matches Descriptor because a TreeEndDescriptor
-- can not have further nesting. However, it can have an StrArr
-- value, which we just enter back as a string. It will be parsed
-- properly once again during codegen.
makeFinal :: [(Text, Descriptor)] -> [(Text, Text)]
makeFinal val = do
  myMap val $ \x ->
    case snd x of
      Descriptor d ->
        if fst x == "type"
          then Nothing
          else Just (fst x, d)
      StrArr d -> Just (fst x, T.pack $ show d)

-- parses an object and returns an IntermediateObject
-- or a Final TreeEndDescriptor, where an IntermediateObject
-- is a nested object while a Final TreeEndDescriptor is an object
-- that contains the `type` property
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
