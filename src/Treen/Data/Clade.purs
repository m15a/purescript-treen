-- | Implementations of multi-way tree called *clade*.
-- |
-- | A clade means a sub-tree growing from a common ancestor in a *phylogenetic* tree.
module Treen.Data.Clade
  ( Clade
  , bundle
  , printClade
  ) where

import Prelude
import Control.Comonad.Cofree (head, tail) as C
import Data.Array (fromFoldable) as A
import Data.Foldable (class Foldable, length)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.List (fromFoldable) as L
import Data.Map (Map)
import Data.Map (keys, lookup, fromFoldableWith) as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set (fromFoldable) as S
import Data.String.Util (trimLastEndOfLine)
import Data.Tree (Forest, Tree, mkTree)
import Data.Tuple (Tuple(..))
import Treen.Data.Lineage (Lineage, head, tail)

-- | It is just a wrapper of `Data.Tree (Tree)`, implemented in package
-- | [`purescript-tree-rose`](https://pursuit.purescript.org/packages/purescript-tree-rose/).
newtype Clade = Clade (Tree String)

derive newtype instance Eq Clade

derive newtype instance Ord Clade

instance Show Clade where
  show c = "(Clade " <> printClade c <> ")"

-- | A node in a tree is either root, bro/sis older than the last child,
-- | or the last one. Used for pretty-printing the tree.
data NodeType
  = Root
  | OlderChild
  | LastChild

-- | Print a clade way prettier.
printClade :: Clade -> String
printClade (Clade t) = trimLastEndOfLine $ go t "" Root
  where
  go tree indent nodeType =
    let
      branch = case nodeType of
        OlderChild -> "├── "
        LastChild -> "└── "
        _ -> ""
      node = C.head tree
      andMore = foldlWithIndex (\i s c -> s <> go c indent' (nodeTypeOf i)) "" children
        where
        indent' = indent <> case nodeType of
          OlderChild -> "│   "
          LastChild -> "    "
          _ -> ""
        nodeTypeOf i
          | i == n = LastChild
          | otherwise = OlderChild
        n = length children - 1
        children = C.tail tree
    in
      indent <> branch <> node <> "\n" <> andMore

-- | Make a set of clades from the given lineages.
-- |
-- | The number of clades depends on the number of roots found in the lineages.
-- | For example, the lineages below contain two roots `a` and `b`, indicated by `^`,
-- |
-- |     a → b → d
-- |     ^
-- |     a → b → e
-- |     ^
-- |     b → c
-- |     ^
-- |
-- | and produce two clades:
-- |
-- |     a───b───d
-- |     ^   └───e
-- |     b───c
-- |     ^
bundle :: forall f. Foldable f => f Lineage -> Set Clade
bundle = A.fromFoldable >>> bundle' >>> map Clade >>> S.fromFoldable

-- | Make a `Forest`, i.e., a list of trees, from an array of lineages.
bundle' :: Array Lineage -> Forest String
bundle' lineages = map (\n -> mkTree n (childrenOf n)) roots
  where
  classification = classify lineages
  roots = classification # M.keys # L.fromFoldable
  childrenOf = lineagesOf >>> bundle'
  lineagesOf node = classification # M.lookup node # fromMaybe []

-- | Classify lineages by their roots, i.e., head nodes.
classify :: Array Lineage -> Map String (Array Lineage)
classify = map toPair >>> M.fromFoldableWith (<>)
  where
  toPair l = Tuple (head l) (A.fromFoldable $ tail l)
