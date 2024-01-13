-- | Implementations of multi-way tree called clade.
-- |
-- | NOTE: A clade is a jargon indicating a sub-tree growing from a common ancestor
-- | in a *phylogenetic* tree.
module Treen.Data.Clade
  ( Clade
  , bundle
  , printClade
  ) where

import Prelude
import Data.Foldable (length)
import Data.FoldableWithIndex (foldlWithIndex)
import Control.Comonad.Cofree (head, tail) as C
import Data.List (fromFoldable) as L
import Data.List.Types (List(..))
import Data.Map (Map, keys, lookup, fromFoldableWith) as M
import Data.Maybe (fromMaybe)
import Data.Tree (Forest, Tree, drawTree, mkTree) as T
import Data.Tuple (Tuple(..))
import Treen.Data.Lineage (Lineage, head, tail)

-- | It is just a wrapper of `Data.Tree (Tree)`, implemented in package
-- | [`purescript-tree-rose`](https://pursuit.purescript.org/packages/purescript-tree-rose/).
newtype Clade = Clade (T.Tree String)

derive newtype instance Eq Clade

derive newtype instance Ord Clade

instance Show Clade where
  show c = "(Clade " <> printClade c <> ")"

-- | A node in a tree is either root, bro/sis older than the last child,
-- | or the last one. Used for pretty-printing the tree.
data NodeType
  = Root
  | OlderBroSis
  | LastChild

-- | Print a clade way prettier.
printClade :: Clade -> String
printClade (Clade t) = go t "" Root
  where
  go tree indent nodeType =
    let
      branch = case nodeType of
        OlderBroSis -> "├── "
        LastChild -> "└── "
        _ -> ""
      nodeName = C.head tree
      andMore = foldlWithIndex (\i s c -> s <> go c indent' (nodeTypeOf i)) "" children
        where
        indent' =
          indent <> case nodeType of
            OlderBroSis -> "│   "
            LastChild -> "    "
            _ -> ""
        nodeTypeOf i
          | i == n = LastChild
          | otherwise = OlderBroSis
        children = C.tail tree
        n = length children - 1
    in
      indent <> branch <> nodeName <> "\n" <> andMore

-- | Make a list of clades from a list of lineages.
-- |
-- | The number of clades depends on the number of roots found in the lineages.
-- | For example, the lineages below contain two roots `a` and `b`, denoted by `^`,
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
bundle :: List Lineage -> List Clade
bundle = bundle' >>> map Clade

-- | Make a `Forest`, i.e., a list of trees, from a list of lineages.
bundle' :: List Lineage -> T.Forest String
bundle' lineages = map (\n -> T.mkTree n (childrenOf n)) roots
  where
  classification = classify lineages
  roots = classification # M.keys # L.fromFoldable
  childrenOf = lineagesOf >>> bundle'
  lineagesOf node = classification # M.lookup node # fromMaybe Nil

-- | Classify lineages by their roots, i.e., head nodes.
classify :: List Lineage -> M.Map String (List Lineage)
classify lineages =
  lineages
    # map (\l -> Tuple (head l) (L.fromFoldable $ tail l))
    # M.fromFoldableWith (<>)
