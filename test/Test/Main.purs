module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
-- import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Data.List.NonEmpty.Util.Spec (nonemplistUtilSpec)
import Test.Data.Maybe.Util.Spec (maybeUtilSpec)
import Test.Data.String.Util.Spec (stringUtilSpec)
import Test.Treen.Data.Lineage.Spec (lineageSpec)
import Test.Treen.Data.Clade.Spec (cladeSpec)
import Test.Treen.Data.Tree.Grove.Spec (groveSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  nonemplistUtilSpec
  maybeUtilSpec
  stringUtilSpec
  lineageSpec
  groveSpec
  cladeSpec
