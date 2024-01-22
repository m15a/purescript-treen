module Treen.App.Command
  ( Command(..)
  , runDefaultCommand
  , runOnelineGitLogCommand
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
import Treen.App.Options (Input(..), Tileset, tilesetOf)
import Treen.App.Version (ersion) as V
import Treen.Data.GitLog.Oneline as GLO
import Treen.Data.Lineage (fromString, fromOnelineGitLog) as L
import Treen.Data.Treen (Treen)
import Treen.Data.Treen (bundle, printWith) as T
import Treen.Util.Data.String (lines) as S
import Treen.Util.Node.Stream.Aff (fromString) as SA

data Command
  = VersionCommand
  | DefaultCommand
      { input :: Input
      , delim :: String
      , tileset :: Tileset
      }
  | OnelineGitLogCommand
      { input :: Input
      , tileset :: Tileset
      }

runVersionCommand :: Effect Unit
runVersionCommand = info V.ersion

runDefaultCommand :: { input :: Input, delim :: String, tileset :: Tileset } -> Effect Unit
runDefaultCommand { input, delim, tileset } = launchAff_ do
  lines <- readTextContents UTF8 input <#> A.concatMap S.lines
  let
    makeLineage = L.fromString (Pattern delim)
    treen = T.bundle $ A.mapMaybe makeLineage lines
    out = printTreenWith tileset treen
  write stdout =<< SA.fromString UTF8 (out <> "\n")

runOnelineGitLogCommand :: { input :: Input, tileset :: Tileset } -> Effect Unit
runOnelineGitLogCommand { input, tileset } = launchAff_ do
  lines <- readTextContents UTF8 input <#> A.concatMap S.lines
  let
    makeLineage = L.fromOnelineGitLog <=< GLO.fromString
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
