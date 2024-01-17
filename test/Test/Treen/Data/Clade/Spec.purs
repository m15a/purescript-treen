module Test.Treen.Data.Clade.Spec where

import Prelude
import Data.Foldable (length)
import Data.List (fromFoldable, head) as L
import Data.Maybe.Util (unwrapJust)
import Data.String.Pattern (Pattern(..))
import Data.String.Util (trimMargin)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Data.Lineage (fromString)

import Treen.Data.Clade

cladeSpec :: Spec Unit
cladeSpec = describe "Treen.Data.Clade" do

  it "bundles lineages having multiple roots into multiple clades" do
    let
      sep = Pattern "."
      ls = L.fromFoldable
        [ unwrapJust $ fromString sep "a.b.c"
        , unwrapJust $ fromString sep "a.d"
        , unwrapJust $ fromString sep "b.d.c"
        ]
    length (bundle ls) `shouldEqual` 2

  it "is printed like this" do
    let
      sep = Pattern "/"
      ls = L.fromFoldable
        [ unwrapJust $ fromString sep "a/b/c"
        , unwrapJust $ fromString sep "a"
        , unwrapJust $ fromString sep "a/b/a/b"
        , unwrapJust $ fromString sep "a/d/c"
        ]
      c = unwrapJust $ L.head $ bundle ls
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
    let mkL = fromString (Pattern "/") >>> unwrapJust
        mkC = map mkL >>> L.fromFoldable >>> bundle >>> L.head >>> unwrapJust
        c1 = mkC ["a"]
        c2 = mkC ["b"]
        c3 = mkC ["a/a", "a/c"]
        c4 = mkC ["b/a"]
        c5 = mkC ["b/", "b"]
    (c1 < c2) `shouldEqual` true
    (c3 < c2) `shouldEqual` true
    (c4 > c2) `shouldEqual` true
    (c5 == c2) `shouldEqual` true
