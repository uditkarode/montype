module Parser.Descriptors.Types where

import           Data.Text (Text)
import qualified Data.Text as T

-- A descriptor is a value that can occur on the right hand
-- side (in nested object or in definition) of a schema def
data Descriptor
    -- | for example String in { type: String }
  = Descriptor Text
    -- | for example String in { username: String }
    -- | this is converted to an IntermediateObject
    -- | before being used (except in IntermediateObjects)
  | Shorthand Text
    -- | for example [Number], [ [Number] ], or []
  | Array SchemaArray
    -- | a nested object that doesn't contain a
    -- | `type` descriptor
  | IntermediateObject [(Text, Descriptor)]
    -- | end of nesting, defines the final type
  | Final TreeEndDescriptor
    -- | to be ignored, used to implement trailing comma
  | NoValue
  deriving (Eq, Ord, Show)

-- Arrays in the schema def can have either a
-- TreeEndDescriptor, another array, or nothing
-- inside them
data SchemaArray
  = SchemaArray TreeEndDescriptor
  | Nested SchemaArray
  | NoItems
  deriving (Eq, Ord, Show)

-- A tree end descriptor denotes a final
-- type value that doesn't nest any further
data TreeEndDescriptor
    -- | (type, [(key, value)]), since every valid
    -- |  FinalDescriptor must have a type field
  = TreeEndDescriptor (Text, [(Text, Text)])
    -- | a descriptor of this type will be added to
    -- | the interface as-is (without mapping)
  | Literal Text
  deriving (Eq, Ord, Show)
