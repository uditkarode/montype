module Parser.Descriptors.Object where

import           Utils                    (Parser)

import           Parser.Descriptors.Types (Descriptor)

-- Since an object can contain an array
-- but an array can also contain an
-- object, it was necessary to do a circular
-- import through an hs-boot file
objDescriptor :: Parser Descriptor
