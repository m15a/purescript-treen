-- | Git log data type.
-- |
-- | It follows the [conventional commits](https://www.conventionalcommits.org).
module Treen.Data.GitLog
  ( GitLog(..)
  , fromOnelineString
  , fromOnelineString'
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

newtype GitLog = GitLog
  { hash :: String
  , type_ :: Maybe String
  , scope :: Maybe String
  , bang :: Boolean
  , title :: String
  }

type GitLogPrefix =
  { type_ :: Maybe String
  , scope :: Maybe String
  , bang :: Boolean
  }

derive newtype instance Eq GitLog

derive newtype instance Show GitLog

-- | Read a string as a oneline Git log. Return nothing if fails.
fromOnelineString :: String -> Maybe GitLog
fromOnelineString = fromOnelineString' >>> hush

-- | Returns `Either` for using parser error messages.
fromOnelineString' :: String -> Either ParseError GitLog
fromOnelineString' = runParser parseOnelineGitLog

parseOnelineGitLog :: Parser GitLog
parseOnelineGitLog = do
  skipSpaces
  hash <- parseGitLogHash
  skipSpaces
  { type_, scope, bang } <- try parseGitLogPrefix <|> noPrefix
  skipSpaces
  title <- parseGitLogTitle
  skipSpaces
  pure $ GitLog { hash, type_, scope, bang, title }
  where
  noPrefix = pure { type_: Nothing, scope: Nothing, bang: false }

parseGitLogHash :: Parser String
parseGitLogHash = many1 anyHex <#> S.fromChars

parseGitLogPrefix :: Parser GitLogPrefix
parseGitLogPrefix = do
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

parseGitLogTitle :: Parser String
parseGitLogTitle = many1 anyChar <#> S.fromChars <#> S.trim
