module Treen.App.Commands
  ( runDefault
  , runVersion
  ) where

import Prelude
import Data.List (fromFoldable, mapMaybe) as L
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (info)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (fromStringUTF8, toStringUTF8, readAll, write)
import Treen.Data.Lineage (fromString)
import Treen.Data.Treen (bundle, print) as T
import Treen.Util.Data.String (lines)
import Treen.Version (ersion) as V

runVersion :: Effect Unit
runVersion = info V.ersion

runDefault :: String -> Effect Unit
runDefault delim = launchAff_ do
  ss <- readAll stdin >>= toStringUTF8 <#> lines <#> L.fromFoldable
  let treen = T.bundle $ L.mapMaybe (fromString $ Pattern delim) ss
  write stdout =<< fromStringUTF8 (T.print treen <> "\n")
