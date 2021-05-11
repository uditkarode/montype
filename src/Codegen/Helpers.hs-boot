module Codegen.Helpers where

import           Data.Map                 as M (Map)
import           Data.Text                (Text)

import           Parser.Descriptors.Types (Descriptor)

-- necessary for cyclic import of Helpers.hs

myFoldM :: (Foldable t, Monad m) => t a -> b -> (b -> a -> m b) -> m b
getTsType :: Descriptor -> Text -> Int -> M.Map Text Text -> Either String Text
getTsType' :: Descriptor -> Int -> M.Map Text Text -> Either String Text
tab :: Text
