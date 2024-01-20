module Treen.App.Options
  ( Options(..)
  , parserInfo
  ) where

import Prelude
import Data.Foldable (fold)
import Options.Applicative
  ( (<**>)
  , Parser
  , ParserInfo
  , header
  , fullDesc
  , help
  , helper
  , info
  , long
  , metavar
  , short
  , showDefault
  , strOption
  , switch
  , value
  )

parserInfo :: ParserInfo Options
parserInfo = info (optionsSpec <**> helper) $ fold
  [ header "treen - print anything as tree-like format"
  , fullDesc
  ]

data Options = Options
  { version :: Boolean
  , delim :: String
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

  in
    Options
      { version
      , delim
      }
