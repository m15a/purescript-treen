module Test.Treen.Util.Data.List.NonEmptySpec (nonemplistUtilSpec) where

import Prelude
import Data.List.NonEmpty (fromFoldable) as L1
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Util.Data.Maybe (unwrapJust)

import Treen.Util.Data.List.NonEmpty

nonemplistUtilSpec :: Spec Unit
nonemplistUtilSpec = describe "Data.List.NonEmpty.Util" do

  describe "tailOfMoreThanOne" do

    it "gets the tail of non-empty list of more-than-one length" do
      let
        xs = unwrapJust $ L1.fromFoldable [ "a", "b" ]
        actual = tailOfMoreThanOne xs
        expected = unwrapJust $ L1.fromFoldable [ "b" ]
      actual `shouldEqual` expected
