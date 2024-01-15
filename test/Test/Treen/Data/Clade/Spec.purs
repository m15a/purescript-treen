module Test.Treen.Data.Clade.Spec where

import Prelude
import Data.Foldable (length)
import Data.List (fromFoldable, head) as L
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Data.Lineage (fromString)
import Treen.Util (unwrapJust)

import Treen.Data.Clade

cladeSpec :: Spec Unit
cladeSpec = describe "Treen.Data.Clade" do

  it "should bundle lineages having multiple roots into multiple clades" do
    let
      sep = Pattern "."
      ls = L.fromFoldable
        [ unwrapJust $ fromString sep "a.b.c"
        , unwrapJust $ fromString sep "a.d"
        , unwrapJust $ fromString sep "b.d.c"
        ]
    length (bundle ls) `shouldEqual` 2

  it "should be printed like this" do
    let
      sep = Pattern "/"
      ls = L.fromFoldable
        [ unwrapJust $ fromString sep "a/b/c"
        , unwrapJust $ fromString sep "a"
        , unwrapJust $ fromString sep "a/b/a/b"
        , unwrapJust $ fromString sep "a/d/c"
        ]
      c = unwrapJust $ L.head $ bundle ls
      likeThis = "a\n"
              <> "├── b\n"
              <> "│   ├── a\n"
              <> "│   │   └── b\n"
              <> "│   └── c\n"
              <> "└── d\n"
              <> "    └── c\n"
    printClade c `shouldEqual` likeThis
