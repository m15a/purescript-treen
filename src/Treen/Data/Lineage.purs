-- | Implementations of lineage.
-- |
-- | A lineage represents a sequence of decent from ancestor to descendant.
-- | For example, a file system path `/a/b/c` can be considered as a lineage
-- |
-- | ```
-- | (root) → a → b → c
-- | ```
-- |
-- | in which the `(root)` is the most ancestor, `a` is its only child,
-- | `b` is `a`'s child as well as `(root)`'s grand child, and so on.
module Treen.Data.Lineage
  ( Lineage(..)
  , fromFoldable
  , fromString
  , head
  , tail
  ) where

import Prelude
import Data.Array as A
import Data.Foldable (class Foldable, length)
import Data.List.NonEmpty (fromFoldable, fromList, head, tail) as L1
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..))
import Data.Maybe.Util (unwrapJust)
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern)

-- | A lineage is a sequence of descent, aligned from ancestor to descendant.
-- |
-- | Don't use its bare constructor even if it is exposed.
-- | Use `fromFoldable` or `fromString` instead.
data Lineage = Lineage Int (NonEmptyList String)

-- | Make a lineage from a foldable thing that wraps `String`.
-- |
-- | Example:
-- |
-- | ```purescript
-- | import Treen.Data.Lineage (fromFoldable)
-- |
-- | fromFoldable ["a", "b"]
-- | ```
fromFoldable :: forall f. Foldable f => f String -> Maybe (Lineage)
fromFoldable xs = do
  let n = length xs
  descents <- L1.fromFoldable xs
  pure $ Lineage n descents

-- | Make a lineage from a string separated by the given separator `sep`.
-- |
-- | Example:
-- |
-- | ```purescript
-- | import Treen.Data.Lineage (fromString)
-- | import Data.String.Pattern (Pattern(..))
-- |
-- | fromString (Pattern ".") "a.b.c"
-- | ```
fromString :: Pattern -> String -> Maybe Lineage
fromString sep = split sep >>> fromFoldable

instance Eq Lineage where
  eq (Lineage m xs) (Lineage n ys)
    | m /= n = false
    | xs == ys = true
    | otherwise = false

instance Ord Lineage where
  compare (Lineage 1 xs) (Lineage 1 ys)
    | L1.head xs < L1.head ys = LT
    | L1.head xs > L1.head ys = GT
    | otherwise = EQ
  compare (Lineage 1 xs) (Lineage _ ys)
    | L1.head xs > L1.head ys = GT
    | otherwise = LT
  compare (Lineage _ xs) (Lineage 1 ys)
    | L1.head xs < L1.head ys = LT
    | otherwise = GT
  compare (Lineage m xs) (Lineage n ys)
    | L1.head xs < L1.head ys = LT
    | L1.head xs > L1.head ys = GT
    | otherwise = compare (Lineage (m - 1) xs') (Lineage (n - 1) ys')
        where
        -- As the lengths of xs and ys are known to be larger than 1,
        -- it's safe to unwrapJust them.
        xs' = tailOfMoreThanOne xs
        ys' = tailOfMoreThanOne ys

instance Show Lineage where
  show (Lineage _ ss) = "(Lineage " <> ss' <> ")"
    where
    ss' = joinWith " → " $ A.fromFoldable ss

-- | Get the head of the lineage.
head :: Lineage -> String
head (Lineage _ xs) = L1.head xs

-- | Get the tail of the lineage, possibly nothing.
tail :: Lineage -> Maybe Lineage
tail (Lineage n xs)
  | n == 1 = Nothing
  | otherwise = Just (Lineage (n - 1) $ tailOfMoreThanOne xs)

-- | Get the tail of a non-empty list, trusting that it has at least two contents.
tailOfMoreThanOne :: NonEmptyList ~> NonEmptyList
tailOfMoreThanOne = unwrapJust <<< L1.fromList <<< L1.tail
