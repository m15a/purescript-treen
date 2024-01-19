module Treen.App.Main (main) where

import Prelude
import Data.Foldable (for_)
import Data.List (fromFoldable, mapMaybe) as L
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (fromStringUTF8, toStringUTF8, readAll, write)
import Treen.Data.Clade (bundle, printClade)
import Treen.Data.Lineage (fromString)
import Treen.Util.Data.String (lines)

main :: Effect Unit
main = launchAff_ do
  ss <- readAll stdin >>= toStringUTF8 <#> lines <#> L.fromFoldable
  let clades = bundle $ L.mapMaybe (fromString $ Pattern "/") ss
  for_ clades \c -> do
    write stdout =<< fromStringUTF8 (printClade c <> "\n")

-- vim: ft=haskell tw=88
