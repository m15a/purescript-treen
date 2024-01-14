module Test.Treen.Data.Clade.Spec where

import Prelude
import Data.Foldable (length)
import Data.List (fromFoldable, head) as L
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Data.Lineage (fromString)
import Treen.Util (unwrap)

import Treen.Data.Clade

cladeSpec :: Spec Unit
cladeSpec = describe "Treen.Data.Clade" do

  it "should bundle lineages having multiple roots into multiple clades" do
    let
      sep = Pattern "."
      ls = L.fromFoldable
        [ unwrap $ fromString sep "a.b.c"
        , unwrap $ fromString sep "a.d"
        , unwrap $ fromString sep "b.d.c"
        ]
    length (bundle ls) `shouldEqual` 2

  it "should be printed like this" do
    let
      sep = Pattern "/"
      ls = L.fromFoldable
        [ unwrap $ fromString sep "a/b/c"
        , unwrap $ fromString sep "a"
        , unwrap $ fromString sep "a/b/a/b"
        , unwrap $ fromString sep "a/d/c"
        ]
      c = unwrap $ L.head $ bundle ls
      likeThis = "a\n"
              <> "├── b\n"
              <> "│   ├── a\n"
              <> "│   │   └── b\n"
              <> "│   └── c\n"
              <> "└── d\n"
              <> "    └── c\n"
    printClade c `shouldEqual` likeThis
