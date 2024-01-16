module Test.Data.List.NonEmpty.Util.Spec where

import Prelude
import Data.List.NonEmpty (fromFoldable) as L1
import Data.Maybe.Util (unwrapJust)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.List.NonEmpty.Util

nonemplistUtilSpec :: Spec Unit
nonemplistUtilSpec = describe "Data.List.NonEmpty.Util" do

  describe "tailOfMoreThanOne" do

    it "gets the tail of non-empty list of more-than-one length" do
      let
        xs = unwrapJust $ L1.fromFoldable [ "a", "b" ]
        actual = tailOfMoreThanOne xs
        expected = unwrapJust $ L1.fromFoldable [ "b" ]
      actual `shouldEqual` expected
