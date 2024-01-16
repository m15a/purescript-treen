-- | Miscellaneous helper functions for `Maybe` type manipulation.
module Data.Maybe.Util
  ( unwrapJust
  ) where

import Prelude
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

-- | unwrapJust a `Maybe` data, trusting that it has some content.
unwrapJust :: forall a. Maybe a -> a
unwrapJust x = unsafePartial $ fromJust x
