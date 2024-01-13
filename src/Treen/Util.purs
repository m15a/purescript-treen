-- | Miscellaneous helper functions.
module Treen.Util where

import Prelude
import Data.Maybe (Maybe, fromJust)
import Partial.Unsafe (unsafePartial)

-- | Unwrap a `Maybe` stuff, trusting that it has some content.
unwrap :: forall a. Maybe a -> a
unwrap x = unsafePartial $ fromJust x
