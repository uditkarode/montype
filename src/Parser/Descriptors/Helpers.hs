{-# LANGUAGE OverloadedStrings #-}
module Parser.Descriptors.Helpers where

import           Data.Maybe               (mapMaybe)
import qualified Data.Text                as T
import           Parser.Descriptors.Types (Descriptor (Descriptor, Final),
                                           TreeEndDescriptor (Literal, TreeEndDescriptor))
import           Parser.Miscellaneous     (identifier, s)
import           Text.Megaparsec          as P (MonadParsec (lookAhead, try),
                                                anySingle, manyTill, (<|>))
import           Text.Megaparsec.Char     as PStr (char, string)
import           Utils                    (Parser)

-- searches for a in [(a, b)]
-- and returns Just b if found
search :: Eq a => a -> [(a, b)] -> Maybe b
search a [] = Nothing
search a (x:xs)
  | fst x == a = pure $ snd x
  | otherwise = search a xs

-- map but with the function coming later
myMap :: [a] -> (a -> Maybe b) -> [b]
myMap f xs = mapMaybe xs f

-- parses everything till a comma or closing curly
-- brace and returns a Descriptor
justDescriptor :: Parser Descriptor
justDescriptor =
  Descriptor . T.pack <$> P.anySingle `P.manyTill` (s >> lookAhead (PStr.char ',' <|> PStr.char '}'))

-- parses an identifier and returns a TreeEndDescriptor
-- with the type set to the matched identifier and an
-- empty list as other props, used to parse shorthand
-- notation where X means { type: X }
shorthandDescriptor :: Parser Descriptor
shorthandDescriptor = do
  i <- identifier
  pure $ Final $ TreeEndDescriptor (i, [])

-- parses Schema.Types.X or mongoose.Schema.Types.X
-- and returns Types.X -- this is to remove ambiguity
-- about the way mongoose is imported
schemaTypeDescriptor :: Parser Descriptor
schemaTypeDescriptor = do
  PStr.string "Schema.Types." <|> PStr.string "mongoose.Schema.Types."
  res <- identifier
  pure $ Final $ Literal $ "mongoose.Schema.Types." <> res

-- matches any one of the arguments or schemaTypeDescriptor
-- the params weren't supposed to be arguments since they'll
-- always be the same, but since modules can't have circular
-- links, this was necessary
anyDescriptor ::
     Parser Descriptor
  -> Parser Descriptor
  -> Parser Descriptor
  -> Parser Descriptor
anyDescriptor objDescriptor arrayDescriptor fallbackDescriptor =
  try objDescriptor <|> try arrayDescriptor <|> try schemaTypeDescriptor <|>
  fallbackDescriptor
