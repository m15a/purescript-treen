module Test.Treen.Data.CommitLog.OnelineSpec (onelineCommitLogSpec) where

import Prelude (Unit, discard)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Treen.Data.CommitLog.Oneline

onelineCommitLogSpec :: Spec Unit
onelineCommitLogSpec = describe "Treen.Data.CommitLog.Oneline" do

  it "understands plain oneline commit log" do
    let
      actual = fromString "36c8b39 Initial commit"
      expected = OnelineCommitLog
        { hash: "36c8b39"
        , type_: Nothing
        , scope: Nothing
        , bang: false
        , title: "Initial commit"
        }
    actual `shouldEqual` Just expected

  it "understands oneline commit log with basic prefix" do
    let
      actual = fromString "36c8b39 fix: something"
      expected = OnelineCommitLog
        { hash: "36c8b39"
        , type_: Just "fix"
        , scope: Nothing
        , bang: false
        , title: "something"
        }
    actual `shouldEqual` Just expected

  it "understands oneline commit log with bang in prefix" do
    let
      actual = fromString "36c8b39 fix!: breaks something"
      expected = OnelineCommitLog
        { hash: "36c8b39"
        , type_: Just "fix"
        , scope: Nothing
        , bang: true
        , title: "breaks something"
        }
    actual `shouldEqual` Just expected

  it "understands oneline commit log with fully-featured prefix" do
    let
      actual = fromString "36c8b39 fix(release)!: breaks something"
      expected = OnelineCommitLog
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
      expected = OnelineCommitLog
        { hash: "36c8b39"
        , type_: Nothing
        , scope: Nothing
        , bang: false
        , title: ":what are these colon parens?:"
        }
    actual `shouldEqual` Just expected
