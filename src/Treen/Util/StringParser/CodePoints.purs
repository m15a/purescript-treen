-- | Miscellaneous character parsers.
module Treen.Util.StringParser.CodePoints
  ( anyHex
  ) where

import Control.Alt ((<|>))
import Data.String.CodeUnits (toCharArray) as S
import StringParser.CodePoints (anyDigit, oneOf)
import StringParser.Parser (Parser)

anyHex :: Parser Char
anyHex = anyDigit <|> oneOf (S.toCharArray "abcdefABCDEF")
