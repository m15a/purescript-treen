-- | Miscellaneous helper functions.
module Treen.Util where

import Prelude
import Data.Maybe (Maybe, fromJust)
import Data.String.Regex (split)
import Data.String.Regex.Flags (global)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

-- | Unwrap a `Maybe` data, trusting that it has some content.
unwrap :: forall a. Maybe a -> a
unwrap x = unsafePartial $ fromJust x

-- | Split a string into lines separated by newline.
lines :: String -> Array String
lines = split byNewline
  where
  byNewline = unsafeRegex "\r\n|\r|\n" global
