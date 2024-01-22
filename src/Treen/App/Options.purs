module Treen.App.Options
  ( Input(..)
  , Mode(..)
  , CommitLogFormat(..)
  , Options(..)
  , Tileset(..)
  , parserInfo
  , tilesetOf
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List.Types (List)
import Options.Applicative
  ( (<**>)
  , Parser
  , ParserInfo
  , ReadM
  , argument
  , eitherReader
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , many
  , metavar
  , option
  , short
  , showDefault
  , str
  , strOption
  , switch
  , value
  )
import Treen.Data.Tileset
  ( Tileset
  , tree
  , colon
  ) as TS

parserInfo :: ParserInfo Options
parserInfo = info (optionsSpec <**> helper) $ fold
  [ header "treen - print anything as tree-like format"
  , fullDesc
  ]

data Mode
  = DefaultMode
  | CommitLogMode

instance Show Mode where
  show DefaultMode = "default"
  show CommitLogMode = "commitlog"

mode_ :: ReadM Mode
mode_ = eitherReader case _ of
  "default" -> Right DefaultMode
  "commitlog" -> Right CommitLogMode
  _ -> Left "invalid mode"

data CommitLogFormat = OnelineCommitLogFormat

instance Show CommitLogFormat where
  show OnelineCommitLogFormat = "oneline"

commitLogFormat_ :: ReadM CommitLogFormat
commitLogFormat_ = eitherReader case _ of
  "oneline" -> Right OnelineCommitLogFormat
  _ -> Left "invalid commit log format"

data Tileset
  = TreeTileset
  | ColonTileset

instance Show Tileset where
  show TreeTileset = "tree"
  show ColonTileset = "colon"

tileset_ :: ReadM Tileset
tileset_ = eitherReader case _ of
  "tree" -> Right TreeTileset
  "colon" -> Right ColonTileset
  _ -> Left "invalid tileset"

tilesetOf :: Tileset -> TS.Tileset
tilesetOf TreeTileset = TS.tree
tilesetOf ColonTileset = TS.colon

data Input
  = StdinInput
  | FilesInput (List String)

data Options = Options
  { version :: Boolean
  , mode :: Mode
  , delim :: String
  , commitLogFormat :: CommitLogFormat
  , tileset :: Tileset
  , args :: List String
  }

optionsSpec :: Parser Options
optionsSpec = ado

  version <- switch $ fold
    [ long "version"
    , short 'V'
    , help "Show version and exit"
    ]

  mode <- option mode_ $ fold
    [ long "mode"
    , short 'm'
    , metavar "MODE"
    , help "Use MODE (default|commitlog) to parse inputs"
    , showDefault
    , value DefaultMode
    ]

  delim <- strOption $ fold
    [ long "delimiter"
    , short 'd'
    , metavar "DELIM"
    , help "Use DELIM to delimit tokens in each line in default mode"
    , showDefault
    , value "/"
    ]

  commitLogFormat <- option commitLogFormat_ $ fold
    [ long "commitlog-format"
    , metavar "FORMAT"
    , help "Use FORMAT (oneline) to parse commit log inputs"
    , showDefault
    , value OnelineCommitLogFormat
    ]

  tileset <- option tileset_ $ fold
    [ long "tileset"
    , short 't'
    , metavar "TILESET"
    , help "Use TILESET (tree|colon) to print trees"
    , showDefault
    , value TreeTileset
    ]

  args <- many $ argument str $ metavar "FILES..."

  in
    Options
      { version
      , mode
      , delim
      , commitLogFormat
      , tileset
      , args
      }
