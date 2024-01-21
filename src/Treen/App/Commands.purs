module Treen.App.Commands
  ( runDefault
  , runVersion
  ) where

import Prelude
import Data.List (fromFoldable, mapMaybe) as L
import Data.String.Common (trim) as S
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (info)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (fromStringUTF8, toStringUTF8, readAll, write)
import Treen.App.Options (Tileset, tilesetOf)
import Treen.Data.Lineage (fromString)
import Treen.Data.Treen (bundle, printWith) as T
import Treen.Util.Data.String (lines)
import Treen.Version (ersion) as V

runVersion :: Effect Unit
runVersion = info V.ersion

runDefault :: { delim :: String, tileset :: Tileset } -> Effect Unit
runDefault { delim, tileset } = launchAff_ do
  ss <- readAll stdin >>= toStringUTF8 <#> lines <#> L.fromFoldable
  let
    treen = T.bundle $ L.mapMaybe (fromString $ Pattern delim) ss
    tileset' = tilesetOf tileset
  write stdout =<< fromStringUTF8 (T.printWith tileset' S.trim treen <> "\n")
