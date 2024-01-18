-- | Implementation of a multi-way tree with edge labels.
-- |
-- | An example shown below, where `∘` represents a node and `─x─` represents
-- | an edge labeled `x`.
-- |
-- | ```
-- | ∘─A─∘
-- | │   └─C─ ...
-- | └─B─∘
-- |     ├─D─ ...
-- |     └─E─ ...
-- | ```
-- |
-- | Ignoring the labelless root node, it can also be interpreted as a bundle
-- | of multiple trees that have labeled nodes:
-- |
-- | ```
-- |     A
-- |     └───C...
-- |     B
-- |     ├───D...
-- |     └───E...
-- | ```
module Treen.Data.Tree.Grove
  ( Grove
  , cons
  , empty
  , fromFoldable
  , insert
  , merge
  , printGrove
  , singleton
  ) where

import Prelude
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Map (Map)
import Data.Map (empty, member, insert, insertWith, singleton, size, toUnfoldable) as M
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as S
import Data.String.Util (trimLastEndOfLine) as S
import Data.Tuple (Tuple(..))

-- | Edge-labeld multi-way tree.
-- | It is just a mapping from labels of edges to corresponding child nodes.
newtype Grove a = Grove (Map a (Grove a))

instance Eq a => Eq (Grove a) where
  eq (Grove x) (Grove y) = eq x y

instance Ord a => Ord (Grove a) where
  compare (Grove x) (Grove y) = compare x y

instance Show a => Show (Grove a) where
  show (Grove g) = "(Grove " <> show g <> ")"

-- | Print a grove, interpreted as a bundle of node-labeled trees.
printGrove :: forall a. Show a => Ord a => Grove a -> String
printGrove (Grove grove) =
  grove
    # M.toUnfoldable
    # map printTree
    # S.joinWith "\n"

-- | A node in a tree is either root, older sibling, or the last child.
-- | Used for pretty-printing the tree.
data NodeType
  = Root
  | OlderChild
  | LastChild

printTree :: forall a. Show a => Ord a => Tuple a (Grove a) -> String
printTree = go Root "" >>> S.trimLastEndOfLine
  where
  go nodeType indent (Tuple node (Grove children)) =
    let
      branch = case nodeType of
        OlderChild -> "├── "
        LastChild -> "└── "
        _ -> ""
      andMore = foldlWithIndex (\i s t -> s <> go (nodeTypeOf i) indent' t) "" children'
        where
        nodeTypeOf i =
          if i < n then OlderChild
          else LastChild
        n = M.size children - 1
        indent' = indent <>
          case nodeType of
            OlderChild -> "│   "
            LastChild -> "    "
            _ -> ""
        children' = M.toUnfoldable children :: Array _
    in
      indent <> branch <> show node <> "\n" <> andMore

-- | Insert an edge directed to a child grove into another grove.
insert :: forall a. Ord a => a -> Grove a -> Grove a -> Grove a
insert edge child (Grove grove) =
  Grove $
    if M.member edge grove then M.insertWith merge edge child grove
    else M.insert edge child grove

-- | Merge two groves into a single grove.
merge :: forall a. Ord a => Grove a -> Grove a -> Grove a
merge old (Grove new) = foldrWithIndex insert old new

instance Ord a => Semigroup (Grove a) where
  append = merge

-- | Make an empty grove.
empty :: forall a. Grove a
empty = Grove M.empty

-- | Make a grove with a single edge directed to the given grove.
cons :: forall a. a -> Grove a -> Grove a
cons edge grove = Grove $ M.singleton edge grove

-- | Make a grove with a single edge directed to an empty grove.
singleton :: forall a. a -> Grove a
singleton edge = cons edge empty

-- | Make a grove that contains a linear chain of edges from a foldable.
fromFoldable :: forall f a. Foldable f => f a -> Maybe (Grove a)
fromFoldable xs = do
  foldr step Nothing xs
  where
  step x Nothing = Just (singleton x)
  step x (Just g) = Just (cons x g)
