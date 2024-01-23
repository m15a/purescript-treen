module Test.Treen.Data.LineageSpec (lineageSpec) where

import Prelude
import Data.Maybe (Maybe(..))
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Data.GitLog (GitLog(..))
import Treen.Util.Data.Maybe (unwrapJust)

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

  it "can be made from oneline git log" do
    let
      actual = show $ unwrapJust $ fromGitLog $
        GitLog { hash: "123abcd", type_: Just "test", scope: Just "lineage", bang: true, title: "test this" }
      expected = "(Lineage test! → lineage → 123abcd test this)"
    actual `shouldEqual` expected

  it "compares lineages alphabetically then in depth" do
    let mkL = fromString (Pattern "/") >>> unwrapJust
    (mkL "a" < mkL "b") `shouldEqual` true
    (mkL "a/a" < mkL "b") `shouldEqual` true
    (mkL "b/a" > mkL "b") `shouldEqual` true
    (mkL "b/" == mkL "b") `shouldEqual` true

  it "pops its head" do
    let
      mkL = fromFoldable >>> unwrapJust
      actual = head $ mkL [ "a" ]
      expected = "a"
    actual `shouldEqual` expected

  it "pops its tail if any" do
    let
      mkL = fromFoldable >>> unwrapJust
      actual = show $ tail $ mkL [ "a", "b" ]
      expected = "(Just (Lineage b))"
    actual `shouldEqual` expected

  it "does not pop anything if not available" do
    let
      mkL = fromFoldable >>> unwrapJust
      actual = tail $ mkL [ "a" ]
      expected = Nothing
    actual `shouldEqual` expected
