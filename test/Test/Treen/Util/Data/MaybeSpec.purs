module Test.Treen.Util.Data.MaybeSpec (maybeUtilSpec) where

import Prelude
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Treen.Util.Data.Maybe

maybeUtilSpec :: Spec Unit
maybeUtilSpec = describe "Data.Maybe.Util" do

  describe "unwrapJust" do

    it "unwraps content of `Just` data" do
      unwrapJust (Just 1) `shouldEqual` 1
