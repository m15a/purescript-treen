module Treen.App.Main (main) where

import Prelude
import Effect (Effect)
import Options.Applicative (execParser)
import Treen.App.Commands
  ( runDefault
  , runVersion
  )
import Treen.App.Options (Options(..), parserInfo)
import Treen.Version (ersion) as V

main :: Effect Unit
main = parseOptions >>= runCommand

data Command
  = Version { version :: String }
  | Default { delim :: String }

parseOptions :: Effect Command
parseOptions = do
  options <- execParser parserInfo
  pure case options of
    Options { version: true } -> Version { version: V.ersion }
    Options { delim } -> Default { delim }

runCommand :: Command -> Effect Unit
runCommand (Version { version }) = runVersion version
runCommand (Default { delim }) = runDefault delim
