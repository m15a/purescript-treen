module Treen.App.Options
  ( Input(..)
  , Mode(..)
  , GitLogFormat(..)
  , Options(..)
  , Tileset(..)
  , parserInfo
  , tilesetOf
  ) where

import Prelude
import Data.Either (Either(..))
import Data.Foldable (fold)
import Data.List.Types (List)
import Node.Path (FilePath)
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
  [ header "treen - print trees from anything"
  , fullDesc
  ]

data Mode
  = DefaultMode
  | GitLogMode

instance Show Mode where
  show DefaultMode = "default"
  show GitLogMode = "gitlog"

mode_ :: ReadM Mode
mode_ = eitherReader case _ of
  "default" -> Right DefaultMode
  "gitlog" -> Right GitLogMode
  _ -> Left "invalid mode"

data GitLogFormat = OnelineGitLogFormat

instance Show GitLogFormat where
  show OnelineGitLogFormat = "oneline"

gitLogFormat_ :: ReadM GitLogFormat
gitLogFormat_ = eitherReader case _ of
  "oneline" -> Right OnelineGitLogFormat
  _ -> Left "invalid Git log format"

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
  | FilesInput (Array FilePath)

data Options = Options
  { version :: Boolean
  , mode :: Mode
  , tileset :: Tileset
  , delim :: String
  , gitLogFormat :: GitLogFormat
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
    , help "Use MODE (default|gitlog) to parse inputs"
    , showDefault
    , value DefaultMode
    ]

  tileset <- option tileset_ $ fold
    [ long "tileset"
    , short 't'
    , metavar "TILESET"
    , help "Use TILESET (tree|colon) to print trees"
    , showDefault
    , value TreeTileset
    ]

  delim <- strOption $ fold
    [ long "delimiter"
    , short 'd'
    , metavar "DELIM"
    , help "Use DELIM to delimit tokens in each line in default mode"
    , showDefault
    , value "/"
    ]

  gitLogFormat <- option gitLogFormat_ $ fold
    [ long "gitlog-format"
    , metavar "FORMAT"
    , help "Use FORMAT (oneline) to parse Git log inputs"
    , showDefault
    , value OnelineGitLogFormat
    ]

  args <- many $ argument str $ metavar "FILES..."

  in
    Options
      { version
      , mode
      , tileset
      , delim
      , gitLogFormat
      , args
      }
