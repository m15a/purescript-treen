-- | Miscellaneous helper functions.
module Treen.Util where

import Prelude
import Data.Maybe (Maybe, fromJust)
import Data.Foldable (class Foldable)
import Data.List.Types (List)
import Data.List (fromFoldable) as L
import Data.Set (fromFoldable) as S
import Partial.Unsafe (unsafePartial)

-- | Unwrap a `Maybe`, trusting that it has some content.
unwrap :: forall a. Maybe a -> a
unwrap x = unsafePartial $ fromJust x

-- | Drop all duplicates from the given foldable.
uniq :: forall f a. Foldable f => Ord a => f a -> List a
uniq = S.fromFoldable >>> L.fromFoldable
