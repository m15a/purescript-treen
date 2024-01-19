-- | Miscellaneous helper functions for string manipulation.
module Treen.Util.Data.String
  ( lines
  , trimLastEndOfLine
  , trimMargin
  ) where

import Prelude
import Data.Array.NonEmpty (mapMaybe) as A1
import Data.Foldable (minimum)
import Data.Maybe (Maybe, fromMaybe)
import Data.String (length)
import Data.String.Regex (match, replace, split)
import Data.String.Regex.Flags (global, multiline, noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)

-- | See https://github.com/menelaos/purescript-stringutils/blob/v0.0.12/src/Data/String/Utils.purs#L232-L232
-- | and http://www.unicode.org/reports/tr18/#RL1.6
lineBoundary :: String
lineBoundary = "\r\n|[\n\r\x0085\x2028\x2029]"

-- | Split the string into lines separated by their boundaries.
-- | The final end of line is ignored if any.
-- |
-- | Example:
-- |
-- | ```purescript
-- | import Treen.Util.Data.String (lines)
-- |
-- | lines "A\nB\nC\n"  -- returns ["A", "B", "C"]
-- | ```
lines :: String -> Array String
lines = trimLastEndOfLine >>> split lineBoundary'
  where
  lineBoundary' = unsafeRegex lineBoundary global

-- | Remove the last end of line if any.
trimLastEndOfLine :: String -> String
trimLastEndOfLine = replace lastEndOfLine removed
  where
  lastEndOfLine = unsafeRegex ("(?:" <> lineBoundary <> ")$") noFlags
  removed = ""

-- | Remove each line's margin, whose length is determined as the minimum
-- | length of beginning whitespaces in every lines.
-- | In addition, the first empty line is ignored for convenience.
-- |
-- | Example:
-- |
-- | ```purescript
-- | import Treen.Util.Data.String (trimMargin)
-- |
-- | trimMargin
-- |   """
-- |   Line 1
-- |     Line 2
-- |   """  -- returns "Line 1\n  Line 2\n"
-- | ```
trimMargin :: String -> String
trimMargin = trimFirstEmptyLine >>> trimMargin'
  where
  trimMargin' s = fromMaybe s do
    n <- minimumMarginWidth s
    pure $ trimMarginBy n s

-- | Remove the first empty line if any.
trimFirstEmptyLine :: String -> String
trimFirstEmptyLine = replace firstEmptyLine removed
  where
  firstEmptyLine = unsafeRegex ("^[ \t]*(?:" <> lineBoundary <> ")") noFlags
  removed = ""

minimumMarginWidth :: String -> Maybe Int
minimumMarginWidth s = do
  matches <- match anyMargin s
  minimum $ A1.mapMaybe (map length) matches
  where
  anyMargin = unsafeRegex "^[ \t]+" (global <> multiline)

trimMarginBy :: Int -> String -> String
trimMarginBy n = replace margin removed
  where
  margin = unsafeRegex ("^[ \t]{" <> show n <> "}") (global <> multiline)
  removed = ""
