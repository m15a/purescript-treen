module Treen.App.Commands
  ( Command(..)
  , runDefaultCommand
  , runOnelineCommitLogCommand
  , runVersionCommand
  ) where

import Prelude
import Data.Array (foldM, fromFoldable, mapMaybe) as A
import Data.String.Common (trim) as S
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (info)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (fromStringUTF8, toStringUTF8, readAll, write)
import Treen.App.Options (Input(..), Tileset, tilesetOf)
import Treen.App.Version (ersion) as V
import Treen.Data.CommitLog.Oneline as CLO
import Treen.Data.Lineage (fromString, fromOnelineCommitLog) as L
import Treen.Data.Treen (bundle, printWith) as T
import Treen.Util.Data.String (lines) as S

data Command
  = VersionCommand
  | DefaultCommand { input :: Input, delim :: String, tileset :: Tileset }
  | OnelineCommitLogCommand { input :: Input, tileset :: Tileset }

runVersionCommand :: Effect Unit
runVersionCommand = info V.ersion

runDefaultCommand
  :: { input :: Input, delim :: String, tileset :: Tileset } -> Effect Unit
runDefaultCommand { input, delim, tileset } = launchAff_ do
  ss <- case input of
    StdinInput -> readAll stdin >>= toStringUTF8 <#> S.lines
    FilesInput files -> A.foldM step [] $ A.fromFoldable files
      where
      step a f = do
        s <- readTextFile UTF8 f
        pure $ a <> S.lines s
  let
    treen = T.bundle $ A.mapMaybe (L.fromString $ Pattern delim) ss
    out = T.printWith (tilesetOf tileset) S.trim treen
  write stdout =<< fromStringUTF8 (out <> "\n")

runOnelineCommitLogCommand :: { input :: Input, tileset :: Tileset } -> Effect Unit
runOnelineCommitLogCommand { input, tileset } = launchAff_ do
  ss <- case input of
    StdinInput -> readAll stdin >>= toStringUTF8 <#> S.lines
    FilesInput files -> A.foldM step [] $ A.fromFoldable files
      where
      step a f = do
        s <- readTextFile UTF8 f
        pure $ a <> S.lines s
  let
    treen = T.bundle $ A.mapMaybe (\s -> CLO.fromString s >>= L.fromOnelineCommitLog) ss
    out = T.printWith (tilesetOf tileset) S.trim treen
  write stdout =<< fromStringUTF8 (out <> "\n")
