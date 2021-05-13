{-# LANGUAGE DataKinds #-}

module Parser.Descriptor where

import           Parser.Descriptors.Array   (arrayDescriptor)
import           Parser.Descriptors.Helpers (anyDescriptor, shorthandDescriptor)
import           Parser.Descriptors.Object  (objDescriptor)
import           Parser.Descriptors.Types   (Descriptor)
import           Utils                      (Parser)

-- Parses the right hand side of a
-- property and returns a Descriptor
getDescriptor :: Parser Descriptor
getDescriptor = anyDescriptor objDescriptor arrayDescriptor shorthandDescriptor
