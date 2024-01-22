-- | Git oneline commit log.
-- |
-- | It follows the [conventional commits](https://www.conventionalcommits.org).
module Treen.Data.CommitLog.Oneline
  ( OnelineCommitLog(..)
  , fromString
  , fromString'
  ) where

import Prelude hiding (between)
import Control.Alt ((<|>))
import Data.Either (Either, hush)
import Data.Maybe (Maybe(..))
import Data.String.Common (trim) as S
import StringParser.CodePoints
  ( alphaNum
  , anyChar
  , char
  , noneOf
  , skipSpaces
  )
import StringParser.Combinators (between, many1, optionMaybe, try)
import StringParser.Parser (ParseError, Parser, runParser)
import Treen.Util.Data.String (fromChars) as S
import Treen.Util.StringParser.CodePoints (anyHex)

newtype OnelineCommitLog = OnelineCommitLog
  { hash :: String
  , type_ :: Maybe String
  , scope :: Maybe String
  , bang :: Boolean
  , title :: String
  }

type OnelineCommitLogPrefix =
  { type_ :: Maybe String
  , scope :: Maybe String
  , bang :: Boolean
  }

derive newtype instance Eq OnelineCommitLog

derive newtype instance Show OnelineCommitLog

-- | Read a string as a oneline commit log. Return nothing if fails.
fromString :: String -> Maybe OnelineCommitLog
fromString = fromString' >>> hush

-- | Returns `Either` for using parser error messages.
fromString' :: String -> Either ParseError OnelineCommitLog
fromString' = runParser parseOnelineCommitLog

parseOnelineCommitLog :: Parser OnelineCommitLog
parseOnelineCommitLog = do
  skipSpaces
  hash <- parseOnelineCommitLogHash
  skipSpaces
  { type_, scope, bang } <- try parseOnelineCommitLogPrefix <|> noPrefix
  skipSpaces
  title <- parseOnelineCommitLogTitle
  skipSpaces
  pure $ OnelineCommitLog { hash, type_, scope, bang, title }
  where
  noPrefix = pure { type_: Nothing, scope: Nothing, bang: false }

parseOnelineCommitLogHash :: Parser String
parseOnelineCommitLogHash = many1 anyHex <#> S.fromChars

parseOnelineCommitLogPrefix :: Parser OnelineCommitLogPrefix
parseOnelineCommitLogPrefix = do
  type_ <- many1 alphaNum <#> S.fromChars
  skipSpaces
  scope <- optionMaybe (parseScope <#> S.fromChars <#> S.trim)
  skipSpaces
  bang <- optionMaybe (char '!') <#> hasBang
  skipSpaces
  _ <- char ':'
  pure { type_: Just type_, scope, bang }
  where
  parseScope = between (char '(') (char ')') (many1 $ noneOf [ ')' ])
  hasBang Nothing = false
  hasBang (Just _) = true

parseOnelineCommitLogTitle :: Parser String
parseOnelineCommitLogTitle = many1 anyChar <#> S.fromChars <#> S.trim
