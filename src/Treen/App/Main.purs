module Treen.App.Main (main) where

import Prelude
import Data.Array (fromFoldable) as A
import Data.List.Types (List(Nil))
import Effect (Effect)
import Options.Applicative (execParser)
import Treen.App.Command
  ( Command(..)
  , runDefaultCommand
  , runVersionCommand
  , runOnelineGitLogCommand
  )
import Treen.App.Options
  ( Input(..)
  , Mode(..)
  , GitLogFormat(..)
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
      Options { args: Nil } -> StdinInput
      Options { args } -> FilesInput (A.fromFoldable args)
    command = case options of
      Options { version: true } -> VersionCommand
      Options { mode: DefaultMode, delim, tileset } ->
        DefaultCommand { input, delim, tileset }
      Options { mode: GitLogMode, gitLogFormat: OnelineGitLogFormat, tileset } ->
        OnelineGitLogCommand { input, tileset }
  pure command

runCommand :: Command -> Effect Unit
runCommand VersionCommand = runVersionCommand
runCommand (DefaultCommand options) = runDefaultCommand options
runCommand (OnelineGitLogCommand options) = runOnelineGitLogCommand options
