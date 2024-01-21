module Treen.App.Options
  ( Options(..)
  , Tileset(..)
  , parserInfo
  , tilesetOf
  ) where

import Prelude
import Data.Foldable (fold)
import Data.Either (Either(..))
import Options.Applicative
  ( (<**>)
  , Parser
  , ParserInfo
  , ReadM
  , eitherReader
  , header
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , option
  , short
  , showDefault
  , strOption
  , switch
  , value
  )
import Treen.Data.Tileset (Tileset, tree, colon) as TS

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

data Options = Options
  { version :: Boolean
  , delim :: String
  , tileset :: Tileset
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

  in
    Options
      { version
      , delim
      , tileset
      }
