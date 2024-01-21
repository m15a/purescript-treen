module Treen.App.Main (main) where

import Prelude
import Effect (Effect)
import Options.Applicative (execParser)
import Treen.App.Commands
  ( runDefault
  , runVersion
  )
import Treen.App.Options (Options(..), Tileset, parserInfo)

main :: Effect Unit
main = parseOptions >>= runCommand

data Command
  = Version
  | Default { delim :: String, tileset :: Tileset }

parseOptions :: Effect Command
parseOptions = do
  options <- execParser parserInfo
  pure case options of
    Options { version: true } -> Version
    Options { delim, tileset } -> Default { delim, tileset }

runCommand :: Command -> Effect Unit
runCommand Version = runVersion
runCommand (Default { delim, tileset }) = runDefault { delim, tileset }
