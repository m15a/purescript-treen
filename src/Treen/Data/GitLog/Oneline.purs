-- | Git oneline Git log.
-- |
-- | It follows the [conventional commits](https://www.conventionalcommits.org).
module Treen.Data.GitLog.Oneline
  ( OnelineGitLog(..)
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

newtype OnelineGitLog = OnelineGitLog
  { hash :: String
  , type_ :: Maybe String
  , scope :: Maybe String
  , bang :: Boolean
  , title :: String
  }

type OnelineGitLogPrefix =
  { type_ :: Maybe String
  , scope :: Maybe String
  , bang :: Boolean
  }

derive newtype instance Eq OnelineGitLog

derive newtype instance Show OnelineGitLog

-- | Read a string as a oneline Git log. Return nothing if fails.
fromString :: String -> Maybe OnelineGitLog
fromString = fromString' >>> hush

-- | Returns `Either` for using parser error messages.
fromString' :: String -> Either ParseError OnelineGitLog
fromString' = runParser parseOnelineGitLog

parseOnelineGitLog :: Parser OnelineGitLog
parseOnelineGitLog = do
  skipSpaces
  hash <- parseOnelineGitLogHash
  skipSpaces
  { type_, scope, bang } <- try parseOnelineGitLogPrefix <|> noPrefix
  skipSpaces
  title <- parseOnelineGitLogTitle
  skipSpaces
  pure $ OnelineGitLog { hash, type_, scope, bang, title }
  where
  noPrefix = pure { type_: Nothing, scope: Nothing, bang: false }

parseOnelineGitLogHash :: Parser String
parseOnelineGitLogHash = many1 anyHex <#> S.fromChars

parseOnelineGitLogPrefix :: Parser OnelineGitLogPrefix
parseOnelineGitLogPrefix = do
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

parseOnelineGitLogTitle :: Parser String
parseOnelineGitLogTitle = many1 anyChar <#> S.fromChars <#> S.trim
