-- | Miscellaneous helper functions for `NonEmptyList` manipulation.
module Treen.Util.Data.List.NonEmpty
  ( tailOfMoreThanOne
  ) where

import Prelude
import Data.List.NonEmpty (fromList, tail)
import Data.List.Types (NonEmptyList)
import Treen.Util.Data.Maybe (unwrapJust)

-- | Get the tail of a non-empty list, trusting that it has at least two contents.
tailOfMoreThanOne :: NonEmptyList ~> NonEmptyList
tailOfMoreThanOne = unwrapJust <<< fromList <<< tail
