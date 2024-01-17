module Test.Treen.Data.Lineage.Spec where

import Prelude
import Data.Maybe (Maybe(..))
import Data.Maybe.Util (unwrapJust)
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Treen.Data.Lineage

lineageSpec :: Spec Unit
lineageSpec = describe "Treen.Data.Lineage" do

  it "can be made from non-empty foldable" do
    let
      actual = show $ unwrapJust $ fromFoldable [ "a" ]
      expected = "(Lineage a)"
    actual `shouldEqual` expected

  it "cannot be made from empty foldable" do
    fromFoldable [] `shouldEqual` Nothing

  it "can be made from non-empty string" do
    let
      actual = show $ unwrapJust $ fromString (Pattern "/") "/a/b/c"
      expected = "(Lineage  → a → b → c)"
    actual `shouldEqual` expected

  it "ignores the last separator" do
    let
      actual = show $ unwrapJust $ fromString (Pattern "/") "a/"
      expected = "(Lineage a)"
    actual `shouldEqual` expected

  it "cannot be made from empty string" do
    let
      actual = fromString (Pattern ".") ""
      expected = Nothing
    actual `shouldEqual` expected

  it "compares lineages alphabetically then in depth" do
    let sep = Pattern "/"
    (fromString sep "a" < fromString sep "b") `shouldEqual` true
    (fromString sep "a/a" < fromString sep "b") `shouldEqual` true
    (fromString sep "b/a" > fromString sep "b") `shouldEqual` true
    (fromString sep "b/" == fromString sep "b") `shouldEqual` true

  it "pops its head" do
    let
      actual = head $ unwrapJust $ fromFoldable [ "a" ]
      expected = "a"
    actual `shouldEqual` expected

  it "pops its tail if any" do
    let
      actual = show $ unwrapJust $ tail $ unwrapJust $ fromFoldable [ "a", "b" ]
      expected = "(Lineage b)"
    actual `shouldEqual` expected

  it "does not pop anything if not available" do
    let
      actual = tail $ unwrapJust $ fromFoldable [ "a" ]
      expected = Nothing
    actual `shouldEqual` expected
