-- | Miscellaneous helper functions for `NonEmptyList` manipulation.
module Data.List.NonEmpty.Util
  ( tailOfMoreThanOne
  ) where

import Prelude
import Data.List.NonEmpty (fromList, tail)
import Data.List.Types (NonEmptyList)
import Data.Maybe.Util (unwrapJust)

-- | Get the tail of a non-empty list, trusting that it has at least two contents.
tailOfMoreThanOne :: NonEmptyList ~> NonEmptyList
tailOfMoreThanOne = unwrapJust <<< fromList <<< tail
