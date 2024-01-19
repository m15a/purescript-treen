module Treen.App.Main (main) where

import Prelude
import Data.List (fromFoldable, mapMaybe) as L
import Data.String.Pattern (Pattern(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Process (stdin, stdout)
import Node.Stream.Aff (fromStringUTF8, toStringUTF8, readAll, write)
import Treen.Data.Treen (bundle, print) as T
import Treen.Data.Lineage (fromString)
import Treen.Util.Data.String (lines)

main :: Effect Unit
main = launchAff_ do
  ss <- readAll stdin >>= toStringUTF8 <#> lines <#> L.fromFoldable
  let treen = T.bundle $ L.mapMaybe (fromString $ Pattern "/") ss
  write stdout =<< fromStringUTF8 (T.print treen <> "\n")

-- vim: ft=haskell tw=88
