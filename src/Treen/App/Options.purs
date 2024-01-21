module Treen.App.Options
  ( Input(..)
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

data Tileset
  = Tree
  | Colon

instance Show Tileset where
  show Tree = "tree"
  show Colon = "colon"

tileset_ :: ReadM Tileset
tileset_ = eitherReader case _ of
  "tree" -> Right Tree
  "colon" -> Right Colon
  _ -> Left "invalid tileset"

tilesetOf :: Tileset -> TS.Tileset
tilesetOf Tree = TS.tree
tilesetOf Colon = TS.colon

data Input
  = Stdin
  | Files (List String)

data Options = Options
  { version :: Boolean
  , delim :: String
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

  delim <- strOption $ fold
    [ long "delimiter"
    , short 'd'
    , metavar "DELIM"
    , help "Use DELIM to delimit tokens in each line"
    , showDefault
    , value "/"
    ]

  tileset <- option tileset_ $ fold
    [ long "tileset"
    , short 't'
    , metavar "TILESET"
    , help "Use TILESET (tree|colon) to print trees"
    , showDefault
    , value Tree
    ]

  args <- many $ argument str $ metavar "FILES..."

  in
    Options
      { version
      , delim
      , tileset
      , args
      }
