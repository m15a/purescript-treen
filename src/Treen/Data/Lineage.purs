-- | Lineage represents a sequence of some tokens from ancestor to descendant.
-- |
-- | A file system path `/a/b/c` can be considered as a lineage
-- |
-- | ```
-- | (root) → a → b → c
-- | ```
-- |
-- | where `(root)`, actually a token of empty string `""`, is the most ancestor,
-- | `a` is its child, `b` is `a`'s child, and `c` is `b`'s child.
module Treen.Data.Lineage
  ( Lineage
  , fromFoldable
  , fromString
  , fromOnelineCommitLog
  , head
  , tail
  , toList
  ) where

import Prelude
import Data.Array (fromFoldable, init, last) as A
import Data.Foldable (class Foldable, length)
import Data.List.NonEmpty (fromFoldable, head, toList) as L1
import Data.List.Types (List, NonEmptyList)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String.Common (joinWith, split)
import Data.String.Pattern (Pattern)
import Treen.Util.Data.List.NonEmpty (tailOfMoreThanOne) as L1
import Treen.Data.CommitLog.Oneline (OnelineCommitLog(..))

-- | A lineage.
-- |
-- | Use `fromFoldable` or `fromString` to create an instance.
newtype Lineage = Lineage
  { size :: Int
  , nodes :: NonEmptyList String
  }

-- | Make a lineage from a foldable of strings.
-- | Empty foldable yields nothing.
fromFoldable :: forall f. Foldable f => f String -> Maybe Lineage
fromFoldable xs = do
  nodes <- L1.fromFoldable xs
  let size = length nodes
  pure $ Lineage { size, nodes }

-- | Make a lineage from a string, separating tokens by the given separator `sep`.
-- |
-- | If there is an empty string before the first separator in the string, it will be
-- | regarded as a token of empty string.
-- | If there is an empty string after the last separator, it will be ignored.
-- |
-- | Examples:
-- |
-- | ```purescript
-- | import Treen.Data.Lineage (fromString)
-- | import Data.String.Pattern (Pattern(..))
-- |
-- | fromString (Pattern ".") "a.b"   -- yields lineage a → b, and
-- | fromString (Pattern ".") "a.b."  -- also yields a → b, ignoring the last empty token.
-- | fromString (Pattern ".") ".a.b"  -- yields lineage (empty token) → a → b.
-- | fromString (Pattern ".") "."     -- yields singleton lineage (empty token).
-- | fromString (Pattern ".") ""      -- yields nothing.
-- | ```
fromString :: Pattern -> String -> Maybe Lineage
fromString sep = split sep >>> trimLastEmpty >>> fromFoldable
  where
  trimLastEmpty a = fromMaybe [] do
    x <- A.last a
    if x == "" then A.init a
    else pure a

-- | Make a lineage from a Git oneline commit log.
-- |
-- | Precedence of the attributes are
-- |
-- |     1. `type_` and optional `!`,
-- |     2. `scope`, and finally
-- |     3. `hash` and `title`.
fromOnelineCommitLog :: OnelineCommitLog -> Maybe Lineage
fromOnelineCommitLog log = do
  fromFoldable case log of
    OnelineCommitLog { hash, type_: Nothing, scope: Nothing, bang: _, title } ->
      [ hash <> " " <> title
      ]
    OnelineCommitLog { hash, type_: Just type_', scope: Nothing, bang, title } ->
      [ type_' <> if bang then "!" else ""
      , hash <> " " <> title
      ]
    OnelineCommitLog { hash, type_: Just type_', scope: Just scope', bang, title } ->
      [ type_' <> if bang then "!" else ""
      , scope'
      , hash <> " " <> title
      ]
    OnelineCommitLog { hash, type_: Nothing, scope: Just scope', bang: _, title } ->
      [ scope'
      , hash <> " " <> title
      ]

instance Eq Lineage where
  eq (Lineage { size: m, nodes: xs }) (Lineage { size: n, nodes: ys })
    | m /= n = false
    | otherwise = eq xs ys

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

-- | Convert to a list.
toList :: Lineage -> List String
toList (Lineage { size: _, nodes: xs }) = L1.toList xs
