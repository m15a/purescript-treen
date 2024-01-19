module Test.Treen.Data.CladeSpec (cladeSpec) where

import Prelude
import Data.Foldable (length, minimum)
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Data.Lineage (fromString)
import Treen.Util.Data.Maybe (unwrapJust)
import Treen.Util.Data.String (trimMargin)

import Treen.Data.Clade

cladeSpec :: Spec Unit
cladeSpec = describe "Treen.Data.Clade" do

  it "bundles lineages having multiple roots into multiple clades" do
    let
      mkL = fromString (Pattern ".") >>> unwrapJust
      ls =
        [ mkL "a.b.c"
        , mkL "a.d"
        , mkL "b.d.c"
        ]
    length (bundle ls) `shouldEqual` 2

  it "can be created from a single lineage" do
    let
      mkL = fromString (Pattern ".") >>> unwrapJust
    (show $ plant $ mkL "a.b.c") `shouldEqual` "(Clade a\n└── b\n    └── c)"

  it "is printed like this" do
    let
      mkL = fromString (Pattern "/") >>> unwrapJust
      ls =
        [ mkL "a/b/c"
        , mkL "a"
        , mkL "a/b/a/b"
        , mkL "a/d/c"
        ]
      c = unwrapJust $ minimum $ bundle ls
      likeThis = trimMargin
        """
        a
        ├── b
        │   ├── a
        │   │   └── b
        │   └── c
        └── d
            └── c"""
    printClade c `shouldEqual` likeThis

  it "compares clades alphabetically then in depth" do
    let
      mkL = fromString (Pattern "/") >>> unwrapJust
      mkC = map mkL >>> bundle >>> minimum >>> unwrapJust
      c1 = mkC [ "a" ]
      c2 = mkC [ "b" ]
      c3 = mkC [ "a/a", "a/c" ]
      c4 = mkC [ "b/a" ]
      c5 = mkC [ "b/", "b" ]
    (c1 < c2) `shouldEqual` true
    (c3 < c2) `shouldEqual` true
    (c4 > c2) `shouldEqual` true
    (c5 == c2) `shouldEqual` true
