{-# LANGUAGE DataKinds #-}

module Parser.Descriptor where

import           Text.Parsec.String         as PStr (Parser)
import qualified Text.Parsec.Token          as PTok

import           Parser.Descriptors.Array   (arrayDescriptor)
import           Parser.Descriptors.Helpers (anyDescriptor, shorthandDescriptor)
import           Parser.Descriptors.Object  (objDescriptor)
import           Parser.Descriptors.Types   (Descriptor)
import           Parser.Miscellaneous       ()

-- Parses the right hand side of a
-- property and returns a Descriptor
getDescriptor :: Parser Descriptor
getDescriptor = anyDescriptor objDescriptor arrayDescriptor shorthandDescriptor
