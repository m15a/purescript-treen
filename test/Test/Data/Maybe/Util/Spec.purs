module Test.Data.Maybe.Util.Spec where

import Prelude
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Data.Maybe.Util

maybeUtilSpec :: Spec Unit
maybeUtilSpec = describe "Data.Maybe.Util" do

  describe "unwrapJust" do

    it "unwraps content of `Just` data" do
      unwrapJust (Just 1) `shouldEqual` 1
