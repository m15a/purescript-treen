module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
-- import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Treen.Data.CladeSpec (cladeSpec)
import Test.Treen.Data.LineageSpec (lineageSpec)
import Test.Treen.Data.Tree.GroveSpec (groveSpec)
import Test.Treen.Util.Data.List.NonEmptySpec (nonemplistUtilSpec)
import Test.Treen.Util.Data.MaybeSpec (maybeUtilSpec)
import Test.Treen.Util.Data.StringSpec (stringUtilSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  nonemplistUtilSpec
  maybeUtilSpec
  stringUtilSpec
  lineageSpec
  groveSpec
  cladeSpec
