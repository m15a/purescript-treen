module Treen.App.Main (main) where

import Prelude
import Data.List.Types (List(Nil))
import Effect (Effect)
import Options.Applicative (execParser)
import Treen.App.Commands
  ( Command(..)
  , runDefault
  , runVersion
  )
import Treen.App.Options
  ( Input(..)
  , Options(..)
  , parserInfo
  )

main :: Effect Unit
main = parseOptions >>= runCommand

parseOptions :: Effect Command
parseOptions = do
  options <- execParser parserInfo
  let
    input = case options of
      Options { args: Nil } -> Stdin
      Options { args } -> Files args
    command = case options of
      Options { version: true } -> Version
      Options { delim, tileset } -> Default { input, delim, tileset }
  pure command

runCommand :: Command -> Effect Unit
runCommand Version = runVersion
runCommand (Default options) = runDefault options
