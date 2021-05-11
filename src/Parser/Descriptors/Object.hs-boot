module Parser.Descriptors.Object where

import           Parser.Descriptors.Types (Descriptor)
import           Text.Parsec.String       (Parser)

-- Since an object can contain an array
-- but an array can also contain an
-- object, it was necessary to do a circular
-- import through an hs-boot file
objDescriptor :: Parser Descriptor
