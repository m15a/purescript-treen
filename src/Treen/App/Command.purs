module Treen.App.Command
  ( Command(..)
  , runDefaultCommand
  , runGitLogCommand
  , runVersionCommand
  ) where

import Prelude
import Data.Array (foldM, concatMap, mapMaybe, singleton, snoc) as A
import Data.String.Common (trim) as S
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Console (info)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (readableToString, write)
import Treen.App.Options (Input(..), Tileset, GitLogFormat(..), tilesetOf)
import Treen.App.Version (ersion) as V
import Treen.Data.GitLog (fromOnelineString) as GL
import Treen.Data.Lineage (fromString, fromGitLog) as L
import Treen.Data.Treen (Treen)
import Treen.Data.Treen (bundle, printWith) as T
import Treen.Util.Data.String (lines) as S
import Treen.Util.Node.Stream.Aff (fromString) as SA

data Command
  = VersionCommand
  | DefaultCommand
      { input :: Input
      , tileset :: Tileset
      , delim :: String
      }
  | GitLogCommand
      { input :: Input
      , tileset :: Tileset
      , gitLogFormat :: GitLogFormat
      }

runVersionCommand :: Effect Unit
runVersionCommand = info V.ersion

runDefaultCommand :: { input :: Input, tileset :: Tileset, delim :: String } -> Effect Unit
runDefaultCommand { input, tileset, delim } = launchAff_ do
  lines <- readTextContents UTF8 input <#> A.concatMap S.lines
  let
    makeLineage = L.fromString (Pattern delim)
    treen = T.bundle $ A.mapMaybe makeLineage lines
    out = printTreenWith tileset treen
  write stdout =<< SA.fromString UTF8 (out <> "\n")

runGitLogCommand :: { input :: Input, tileset :: Tileset, gitLogFormat :: GitLogFormat } -> Effect Unit
runGitLogCommand { input, tileset, gitLogFormat: OnelineGitLogFormat } = launchAff_ do
  lines <- readTextContents UTF8 input <#> A.concatMap S.lines
  let
    makeLineage = L.fromGitLog <=< GL.fromOnelineString
    treen = T.bundle $ A.mapMaybe makeLineage lines
    out = printTreenWith tileset treen
  write stdout =<< SA.fromString UTF8 (out <> "\n")

-- | Read text contents in the given encoding from stdin/files.
readTextContents :: Encoding -> Input -> Aff (Array String)
readTextContents encoding = case _ of
  StdinInput -> readableToString stdin encoding <#> A.singleton
  FilesInput files -> A.foldM step [] files
  where
  step arr file = A.snoc arr <$> readTextFile encoding file

-- | Print a treen with the given tileset.
printTreenWith :: Tileset -> Treen -> String
printTreenWith tileset treen = T.printWith (tilesetOf tileset) S.trim treen
