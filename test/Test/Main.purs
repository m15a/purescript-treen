module Test.Main where

import Prelude
import Effect (Effect)
import Effect.Aff (launchAff_)
-- import Test.Spec.Discovery (discover)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

import Test.Treen.Data.GitLogSpec (gitLogSpec)
import Test.Treen.Data.TreenSpec (treenSpec)
import Test.Treen.Data.LineageSpec (lineageSpec)
import Test.Treen.Data.GroveSpec (groveSpec)
import Test.Treen.Util.Data.List.NonEmptySpec (nonEmptyListUtilSpec)
import Test.Treen.Util.Data.MaybeSpec (maybeUtilSpec)
import Test.Treen.Util.Data.StringSpec (stringUtilSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  nonEmptyListUtilSpec
  maybeUtilSpec
  stringUtilSpec
  lineageSpec
  groveSpec
  treenSpec
  gitLogSpec
