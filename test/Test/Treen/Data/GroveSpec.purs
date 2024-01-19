module Test.Treen.Data.GroveSpec (groveSpec) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Util.Data.Maybe (unwrapJust)
import Treen.Util.Data.String (trimMargin)

import Treen.Data.Grove

mkG :: forall f a. Foldable f => f a -> Grove a
mkG = fromFoldable >>> unwrapJust

groveSpec :: Spec Unit
groveSpec = describe "Treen.Data.Grove" do

  it "can be created from a foldable" do
    (print $ mkG [ 1, 2, 3 ]) `shouldEqual` "1\n└── 2\n    └── 3"

  it "can be merged with the other groves" do
    let
      g = foldl (<>) empty
        [ mkG [ "a", "b", "c" ]
        , mkG [ "a" ]
        , mkG [ "a", "b", "a", "b" ]
        , mkG [ "a", "d", "c" ]
        ]
      likeThis = trimMargin
        """
        "a"
        ├── "b"
        │   ├── "a"
        │   │   └── "b"
        │   └── "c"
        └── "d"
            └── "c""""
    print g `shouldEqual` likeThis

  it "compares groves alphabetically then in depth" do
    let
      g1 = mkG [ "a" ]
      g2 = mkG [ "a" ]
      g3 = mkG [ "a", "" ]
      g4 = mkG [ "b" ]
      g5 = mkG [ "a", "c" ] <> mkG [ "b", "a" ]
      g6 = mkG [ "b", "c" ] <> mkG [ "b", "a" ]
    (g1 == g2) `shouldEqual` true
    (g1 < g3) `shouldEqual` true
    (g3 < g4) `shouldEqual` true
    (g1 < g5) `shouldEqual` true
    (g5 < g6) `shouldEqual` true
    (g4 < g6) `shouldEqual` true
