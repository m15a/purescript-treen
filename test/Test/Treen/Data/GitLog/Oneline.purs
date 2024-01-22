module Test.Treen.Data.GitLog.OnelineSpec (onelineGitLogSpec) where

import Prelude (Unit, discard)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Treen.Data.GitLog.Oneline

onelineGitLogSpec :: Spec Unit
onelineGitLogSpec = describe "Treen.Data.GitLog.Oneline" do

  it "understands plain oneline Git log" do
    let
      actual = fromString "36c8b39 Initial commit"
      expected = OnelineGitLog
        { hash: "36c8b39"
        , type_: Nothing
        , scope: Nothing
        , bang: false
        , title: "Initial commit"
        }
    actual `shouldEqual` Just expected

  it "understands oneline Git log with basic prefix" do
    let
      actual = fromString "36c8b39 fix: something"
      expected = OnelineGitLog
        { hash: "36c8b39"
        , type_: Just "fix"
        , scope: Nothing
        , bang: false
        , title: "something"
        }
    actual `shouldEqual` Just expected

  it "understands oneline Git log with bang in prefix" do
    let
      actual = fromString "36c8b39 fix!: breaks something"
      expected = OnelineGitLog
        { hash: "36c8b39"
        , type_: Just "fix"
        , scope: Nothing
        , bang: true
        , title: "breaks something"
        }
    actual `shouldEqual` Just expected

  it "understands oneline Git log with fully-featured prefix" do
    let
      actual = fromString "36c8b39 fix(release)!: breaks something"
      expected = OnelineGitLog
        { hash: "36c8b39"
        , type_: Just "fix"
        , scope: Just "release"
        , bang: true
        , title: "breaks something"
        }
    actual `shouldEqual` Just expected

  it "falls back to plain log when parsing fails" do
    let
      actual = fromString "36c8b39 :what are these colon parens?:"
      expected = OnelineGitLog
        { hash: "36c8b39"
        , type_: Nothing
        , scope: Nothing
        , bang: false
        , title: ":what are these colon parens?:"
        }
    actual `shouldEqual` Just expected
