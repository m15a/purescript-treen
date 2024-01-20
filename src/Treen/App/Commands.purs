module Treen.App.Commands
  ( runDefault
  , runVersion
  ) where

import Prelude
import Data.List (fromFoldable, mapMaybe) as L
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (fromStringUTF8, toStringUTF8, readAll, write)
import Treen.Data.Lineage (fromString)
import Treen.Data.Treen (bundle, print) as T
import Treen.Util.Data.String (lines)

runVersion :: String -> Effect Unit
runVersion version = launchAff_ do
  write stdout =<< fromStringUTF8 (version <> "\n")

runDefault :: String -> Effect Unit
runDefault delim = launchAff_ do
  ss <- readAll stdin >>= toStringUTF8 <#> lines <#> L.fromFoldable
  let treen = T.bundle $ L.mapMaybe (fromString $ Pattern delim) ss
  write stdout =<< fromStringUTF8 (T.print treen <> "\n")
