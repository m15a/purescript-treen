module Test.Data.String.Util.Spec where

import Prelude
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.String.Util

stringUtilSpec :: Spec Unit
stringUtilSpec = describe "Data.String.Util" do

  describe "trimMargin" do

    it "removes the first empty line" do
      let actual = trimMargin
            """  
            
            
            a"""
          expected = "\n\na"
      actual `shouldEqual` expected

    it "removes each line's margin" do
      let actual = trimMargin
            """
             a
            b
               c"""
          expected = " a\nb\n   c"
      actual `shouldEqual` expected

    it "does not remove the last newline" do
      let actual = trimMargin
            """
            a
            
            """
          expected = "a\n\n"
      actual `shouldEqual` expected
