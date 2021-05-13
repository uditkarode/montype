module Parser.Descriptors.Object where

import           Data.Text                as T
import           Data.Void
import           Parser.Descriptors.Types (Descriptor)
import           Text.Megaparsec          as P

type Parser = Parsec Void T.Text

-- Since an object can contain an array
-- but an array can also contain an
-- object, it was necessary to do a circular
-- import through an hs-boot file
objDescriptor :: Parser Descriptor
