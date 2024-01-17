module Test.Data.String.Util.Spec where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.String.Util

stringUtilSpec :: Spec Unit
stringUtilSpec = describe "Data.String.Util" do

  describe "lines" do
    it "splits string by their line boundaries" do
      let
        actual = lines "い\nろ\r\nは\rに\x0085ほ\x2028へ\x2029と"
        expected = [ "い", "ろ", "は", "に", "ほ", "へ", "と" ]
      actual `shouldEqual` expected

    it "ignores empty after the last line boundary" do
      {-
         See Haskell's implementation:
         https://hackage.haskell.org/package/base/docs/Prelude.html#v:lines

            The `\n` terminator is optional in a final non-empty line of
            the argument string.
      -}
      let
        actual = lines "A\n"
        expected = [ "A" ]
      actual `shouldEqual` expected

  describe "trimLastEndOfLine" do
    it "removes the last end of line" do
      let
        actual = trimLastEndOfLine "A\r\n\n"
        expected = "A\r\n"
      actual `shouldEqual` expected

  describe "trimMargin" do

    it "removes the first empty line" do
      let
        actual = trimMargin
          """  
          a"""
        expected = "a"
      actual `shouldEqual` expected

    it "removes each line's margin" do
      let
        actual = trimMargin
          """
           a
          b
             c"""
        expected = " a\nb\n   c"
      actual `shouldEqual` expected

    it "does not remove the last end of line" do
      let
        actual = trimMargin
          """
          a
          """
        expected = "a\n"
      actual `shouldEqual` expected
