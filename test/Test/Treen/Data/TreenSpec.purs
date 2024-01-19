module Test.Treen.Data.TreenSpec (treenSpec) where

import Prelude
import Data.String.Pattern (Pattern(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Treen.Data.Lineage (fromString)
import Treen.Util.Data.Maybe (unwrapJust)
import Treen.Util.Data.String (trimMargin)

import Treen.Data.Treen

treenSpec :: Spec Unit
treenSpec = describe "Treen.Data.Treen" do

  it "is printed like this" do
    let
      mkL = fromString (Pattern "/") >>> unwrapJust
      ls =
        [ mkL "a/b/c"
        , mkL "a"
        , mkL "a/b/a/b"
        , mkL "a/d/c"
        ]
      t = bundle ls
      likeThis = trimMargin
        """
        a
        ├── b
        │   ├── a
        │   │   └── b
        │   └── c
        └── d
            └── c"""
    print t `shouldEqual` likeThis

  it "compares treens alphabetically then in depth" do
    let
      mkL = fromString (Pattern "/") >>> unwrapJust
      mkT = map mkL >>> bundle
      t1 = mkT [ "a" ]
      t2 = mkT [ "b" ]
      t3 = mkT [ "a/a", "a/c" ]
      t4 = mkT [ "b/a" ]
      t5 = mkT [ "b/", "b" ]
    (t1 < t2) `shouldEqual` true
    (t3 < t2) `shouldEqual` true
    (t4 > t2) `shouldEqual` true
    (t5 == t2) `shouldEqual` true
