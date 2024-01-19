-- | Implementation of a multi-way tree with edge names.
-- |
-- | Note that edges growing from the same node cannot have identical names.
-- |
-- | An example shown below, where `∘` represents a node and `─x─` represents
-- | an edge named `x`.
-- |
-- | ```
-- | ∘─A─∘
-- | │   └─C─∘ ...
-- | └─B─∘
-- |     ├─D─∘ ...
-- |     └─E─∘ ...
-- | ```
-- |
-- | Ignoring the nameless root node, it can also be interpreted as a set
-- | of multiple trees that have named nodes:
-- |
-- | ```
-- |     A
-- |     └───C ...
-- |     B
-- |     ├───D ...
-- |     └───E ...
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
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Map (Map)
import Data.Map (empty, insertWith, singleton, size, toUnfoldable) as M
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String (joinWith) as S
import Data.String.Util (trimLastEndOfLine) as S
import Data.Tuple (Tuple(..))

-- | Edge-named multi-way tree.
-- |
-- | It is just a mapping from names of edges to corresponding child nodes.
newtype Grove a = Grove (Map a (Grove a))

instance Eq a => Eq (Grove a) where
  eq (Grove x) (Grove y) = eq x y

derive instance Eq1 Grove

instance Ord a => Ord (Grove a) where
  compare (Grove x) (Grove y) = compare x y

derive instance Ord1 Grove

instance Show a => Show (Grove a) where
  show (Grove g) = "(Grove " <> show g <> ")"

-- | Print a grove, interpreted as node-named trees.
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
  go nodeType header (Tuple node (Grove children)) =
    let
      branch = case nodeType of
        OlderChild -> "├── "
        LastChild -> "└── "
        _ -> ""
      header' = header <>
        case nodeType of
          OlderChild -> "│   "
          LastChild -> "    "
          _ -> ""
      andMore = foldlWithIndex (\i s t -> s <> go (nodeTypeOf i) header' t) "" children'
        where
        nodeTypeOf i =
          if i < n then OlderChild
          else LastChild
        n = M.size children - 1
        children' = M.toUnfoldable children :: Array _
    in
      header <> branch <> show node <> "\n" <> andMore

-- | Insert an edge directed to a child grove into another target grove.
-- |
-- | If an edge of the same name has already been there in the target grove,
-- | the child grove will be merged with that of the target grove.
-- | For example, inserting an edge `A`, directed to a grove that have only
-- | an edge `B`,
-- |
-- | ```
-- | A─∘
-- |   └─B─∘
-- | ```
-- |
-- | into a target grove that already has an edge named `A`
-- |
-- | ```
-- | ∘─A─∘
-- | │   └─C─∘
-- | └─B─∘
-- | ```
-- |
-- | produces a merged grove:
-- |
-- | ```
-- | ∘─A─∘
-- | │   ├─B─∘
-- | │   └─C─∘
-- | └─B─∘
-- | ```
insert :: forall a. Ord a => a -> Grove a -> Grove a -> Grove a
insert edge child (Grove grove) = Grove $ M.insertWith merge edge child grove

-- | Merge two groves into a single grove.
-- |
-- | Simply inserts every edges of a grove one by one into another grove.
merge :: forall a. Ord a => Grove a -> Grove a -> Grove a
merge old (Grove new) = foldrWithIndex insert old new

-- | Merging groves forms a semigroup.
instance Ord a => Semigroup (Grove a) where
  append = merge

-- | Make an empty grove.
empty :: forall a. Grove a
empty = Grove M.empty

instance Ord a => Monoid (Grove a) where
  mempty = empty

-- | Make a grove with a single edge directed to the given grove.
cons :: forall a. a -> Grove a -> Grove a
cons edge grove = Grove $ M.singleton edge grove

-- | Make a grove with a single edge directed to an empty grove.
singleton :: forall a. a -> Grove a
singleton edge = cons edge empty

-- | Make a grove that contains a linear chain of edges from a foldable.
-- |
-- | For example, `fromFoldable [1, 2, 3]` produces a linear grove:
-- |
-- | ```
-- | ∘─1─∘
-- |     └─2─∘
-- |         └─3─∘
-- | ```
fromFoldable :: forall f a. Foldable f => f a -> Maybe (Grove a)
fromFoldable xs = do
  foldr step Nothing xs
  where
  step x Nothing = Just (singleton x)
  step x (Just g) = Just (cons x g)
