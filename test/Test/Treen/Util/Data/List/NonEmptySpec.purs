module Test.Treen.Util.Data.List.NonEmptySpec (nonEmptyListUtilSpec) where

import Prelude
import Data.List.NonEmpty (fromFoldable) as L1
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Util.Data.Maybe (unwrapJust)

import Treen.Util.Data.List.NonEmpty

nonEmptyListUtilSpec :: Spec Unit
nonEmptyListUtilSpec = describe "Treen.Util.Data.List.NonEmpty" do

  describe "tailOfMoreThanOne" do

    it "gets the tail of non-empty list having >=2 items" do
      let
        xs = unwrapJust $ L1.fromFoldable [ "a", "b" ]
        actual = tailOfMoreThanOne xs
        expected = unwrapJust $ L1.fromFoldable [ "b" ]
      actual `shouldEqual` expected
