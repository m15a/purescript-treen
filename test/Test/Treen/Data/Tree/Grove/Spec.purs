module Test.Treen.Data.Tree.Grove.Spec where

import Prelude
import Data.Foldable (foldl)
import Data.Maybe.Util (unwrapJust)
import Data.String.Util (trimMargin)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Treen.Data.Tree.Grove

groveSpec :: Spec Unit
groveSpec = describe "Treen.Data.Tree.Grove" do

  it "can be created from a foldable" do
    let mkG = fromFoldable >>> unwrapJust
    (printGrove $ mkG [ 1, 2, 3 ]) `shouldEqual` "1\n└── 2\n    └── 3"

  it "can be merged with the other groves" do
    let
      mkG = fromFoldable >>> unwrapJust
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
    printGrove g `shouldEqual` likeThis

  it "compares groves alphabetically then in depth" do
    let
      mkG = fromFoldable >>> unwrapJust
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
