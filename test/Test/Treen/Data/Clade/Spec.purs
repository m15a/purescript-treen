module Test.Treen.Data.Clade.Spec where

import Prelude
import Data.Foldable (length)
import Data.List (fromFoldable) as L
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
