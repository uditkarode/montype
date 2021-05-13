{-# LANGUAGE DataKinds #-}

module Parser.Descriptor where

import           Data.Text                  as T
import           Data.Void
import           Text.Megaparsec            as P

import           Parser.Descriptors.Array   (arrayDescriptor)
import           Parser.Descriptors.Helpers (anyDescriptor, shorthandDescriptor)
import           Parser.Descriptors.Object  (objDescriptor)
import           Parser.Descriptors.Types   (Descriptor)
import           Parser.Miscellaneous       ()

type Parser = Parsec Void T.Text

-- Parses the right hand side of a
-- property and returns a Descriptor
getDescriptor :: Parser Descriptor
getDescriptor = anyDescriptor objDescriptor arrayDescriptor shorthandDescriptor
