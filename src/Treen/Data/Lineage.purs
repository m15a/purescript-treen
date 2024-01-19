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
  ( Lineage
  , fromFoldable
  , fromString
  , head
  , tail
  ) where

import Prelude
import Data.Array (fromFoldable, init, last) as A
import Data.Foldable (class Foldable, length)
import Data.List.NonEmpty (fromFoldable, head) as L1
import Data.List.Types (NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern)
import Treen.Util.Data.List.NonEmpty (tailOfMoreThanOne) as L1

-- | A lineage is a sequence of descent, aligned from ancestor to descendant.
-- |
-- | Use `fromFoldable` or `fromString` to create an instance.
newtype Lineage = Lineage
  { size :: Int
  , nodes :: NonEmptyList String
  }

-- | Make a lineage from a foldable that contains `String`.
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
  let size = length xs
  nodes <- L1.fromFoldable xs
  pure $ Lineage { size, nodes }

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
fromString sep = split sep >>> trimLastEmpty >>> fromFoldable
  where
  trimLastEmpty a = fromMaybe [] do
    x <- A.last a
    if x == "" then A.init a
    else pure a

instance Eq Lineage where
  eq (Lineage { size: m, nodes: xs }) (Lineage { size: n, nodes: ys })
    | m /= n = false
    | xs == ys = true
    | otherwise = false

instance Ord Lineage where
  compare (Lineage { size: 1, nodes: xs }) (Lineage { size: 1, nodes: ys })
    | L1.head xs < L1.head ys = LT
    | L1.head xs > L1.head ys = GT
    | otherwise = EQ
  compare (Lineage { size: 1, nodes: xs }) (Lineage { size: _, nodes: ys })
    | L1.head xs > L1.head ys = GT
    | otherwise = LT
  compare (Lineage { size: _, nodes: xs }) (Lineage { size: 1, nodes: ys })
    | L1.head xs < L1.head ys = LT
    | otherwise = GT
  compare (Lineage { size: m, nodes: xs }) (Lineage { size: n, nodes: ys })
    | L1.head xs < L1.head ys = LT
    | L1.head xs > L1.head ys = GT
    | otherwise =
        compare (Lineage { size: m - 1, nodes: xs' })
          (Lineage { size: n - 1, nodes: ys' })
        where
        -- As the lengths of xs and ys are known to be larger than 1,
        -- it's safe to do these.
        xs' = L1.tailOfMoreThanOne xs
        ys' = L1.tailOfMoreThanOne ys

instance Show Lineage where
  show (Lineage { size: _, nodes: ss }) = "(Lineage " <> ss' <> ")"
    where
    ss' = joinWith " → " $ A.fromFoldable ss

-- | Get the head of the lineage.
head :: Lineage -> String
head (Lineage { size: _, nodes: xs }) = L1.head xs

-- | Get the tail of the lineage, possibly nothing.
tail :: Lineage -> Maybe Lineage
tail (Lineage { size: n, nodes: xs })
  | n == 1 = Nothing
  | otherwise = Just (Lineage { size: n - 1, nodes: L1.tailOfMoreThanOne xs })
