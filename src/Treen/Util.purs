-- | Miscellaneous helper functions.
module Treen.Util
  ( unwrapJust
  , lines
  , trimMargin
  ) where

import Prelude
import Data.Foldable (minimum)
import Data.Array.NonEmpty (mapMaybe) as A1
import Data.Maybe (Maybe, fromJust, fromMaybe)
import Data.String (length) as S
import Data.String.Regex (match, replace, split)
import Data.String.Regex.Flags (global, multiline, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Partial.Unsafe (unsafePartial)

-- | unwrapJust a `Maybe` data, trusting that it has some content.
unwrapJust :: forall a. Maybe a -> a
unwrapJust x = unsafePartial $ fromJust x

-- | Split a string into lines separated by newline.
lines :: String -> Array String
lines = split byNewline
  where
  byNewline = unsafeRegex "\r\n|\r|\n" global

-- | Remove the first empty line and each line's margin of fixed length.
-- |
-- | Examples:
-- |
-- | ```purescript
-- | trimMargin
-- |   """
-- |   Line 1
-- |   Line 2
-- |   """
-- | -- returns "Line 1\nLine 2\n"
-- | ```
trimMargin :: String -> String
trimMargin = trimFirstEmptyLine >>> trimMargin'
  where
  trimMargin' s = fromMaybe s do
    n <- minimumMarginWidth s
    pure $ trimMarginBy n s

trimFirstEmptyLine :: String -> String
trimFirstEmptyLine = replace firstEmptyLine removed
  where
  firstEmptyLine = unsafeRegex "^[ \t]*(?:\r\n|\r|\n)" noFlags
  removed = ""

minimumMarginWidth :: String -> Maybe Int
minimumMarginWidth s = do
  matches <- match anyMargin s
  minimum $ A1.mapMaybe (map S.length) matches
  where
  anyMargin = unsafeRegex ("^[ \t]+") (global <> multiline)

trimMarginBy :: Int -> String -> String
trimMarginBy n = replace margin removed
  where
  margin = unsafeRegex ("^[ \t]{" <> show n <> "}") (global <> multiline)
  removed = ""
