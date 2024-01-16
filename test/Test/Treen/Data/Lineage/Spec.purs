module Test.Treen.Data.Lineage.Spec where

import Prelude
import Data.List.NonEmpty as L
import Data.Maybe (Maybe(..))
import Data.Maybe.Util (unwrapJust)
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Treen.Data.Lineage

lineageSpec :: Spec Unit
lineageSpec = describe "Treen.Data.Lineage" do

  it "can be made from non-empty foldable" do
    let xs = unwrapJust $ L.fromFoldable [ "a", "b" ]
    fromFoldable [ "a", "b" ] `shouldEqual` Just (Lineage 2 xs)

  it "can be made from empty foldable" do
    fromFoldable [] `shouldEqual` Nothing

  it "can be made from non-empty string" do
    let xs = unwrapJust $ L.fromFoldable [ "", "a", "b", "c" ]
    fromString (Pattern "/") "/a/b/c" `shouldEqual` Just (Lineage 4 xs)

  it "can also be made from empty string" do
    let xs = unwrapJust $ L.fromFoldable [ "" ]
    fromString (Pattern ".") "" `shouldEqual` Just (Lineage 1 xs)

  it "should compare lineages alphabetically then in depth" do
    let sep = Pattern "/"
    (fromString sep "a/b" < fromString sep "b") `shouldEqual` true
    (fromString sep "a/a" < fromString sep "b") `shouldEqual` true
    (fromString sep "b/a" > fromString sep "b") `shouldEqual` true
    (fromString sep "b/" > fromString sep "b") `shouldEqual` true
    (fromString sep "" < fromString sep "b") `shouldEqual` true
