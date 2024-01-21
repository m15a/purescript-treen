-- | A multi-way tree with named edges, non-named nodes, and no node/edge values.
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
-- |
-- | Doesn't it look like a grove?
-- |
-- | NOTE: Muptiple edges branching from the same node cannot have the same name.
module Treen.Data.Grove
  ( Grove
  , cons
  , empty
  , fromFoldable
  , insert
  , merge
  , print
  , printWith
  , singleton
  ) where

import Prelude
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldr)
import Data.FoldableWithIndex (foldlWithIndex, foldrWithIndex)
import Data.Map (Map)
import Data.Map (empty, insertWith, isEmpty, singleton, size, toUnfoldable) as M
import Data.Maybe (Maybe(..))
import Data.Ord (class Ord1)
import Data.String (joinWith) as S
import Data.Tuple (Tuple(..))
import Treen.Data.Tileset (Tileset)
import Treen.Data.Tileset (tree) as TS
import Treen.Util.Data.String (trimLastEndOfLine) as S

newtype Grove a = Grove (Map a (Grove a))

instance Eq a => Eq (Grove a) where
  eq (Grove x) (Grove y) = eq x y

derive instance Eq1 Grove

instance Ord a => Ord (Grove a) where
  compare (Grove x) (Grove y) = compare x y

derive instance Ord1 Grove

instance Show a => Show (Grove a) where
  show (Grove m) = "(Grove " <> show m <> ")"

-- | Print a grove, interpreted as node-named trees.
print :: forall a. Show a => Ord a => Grove a -> String
print = printWith TS.tree show

-- | Print a grove with a customized tileset and a name formatter.
printWith :: forall a. Ord a => Tileset -> (a -> String) -> Grove a -> String
printWith tileset formatter (Grove m) =
  m # M.toUnfoldable
    # map (printTreeWith tileset formatter)
    # S.joinWith "\n"

-- | A node in a tree is either root, older child, or the last child.
-- | Used for pretty-printing the tree.
data NodeType
  = Root
  | OlderChild
  | LastChild

-- | Print a tree in a grove with a customized tileset and a name formatter.
printTreeWith
  :: forall a. Ord a => Tileset -> (a -> String) -> Tuple a (Grove a) -> String
printTreeWith tileset formatter = go Root "" >>> S.trimLastEndOfLine
  where
  go nodeType header (Tuple name (Grove children)) =
    let
      branch = case nodeType of
        OlderChild -> tileset.olderChildBranch
        LastChild -> tileset.lastChildBranch
        _ -> ""
      isLeaf = M.isEmpty children
      name' =
        if isLeaf then tileset.leafPrefix <> formatter name <> tileset.leafPostfix
        else tileset.branchPrefix <> formatter name <> tileset.branchPostfix
      header' = header <>
        case nodeType of
          OlderChild -> tileset.olderChildHeader
          LastChild -> tileset.lastChildHeader
          _ -> ""
      andMore = foldlWithIndex (\i s t -> s <> go (nodeTypeOf i) header' t) "" children'
        where
        nodeTypeOf i =
          if i < n then OlderChild
          else LastChild
        n = M.size children - 1
        children' = M.toUnfoldable children :: Array _
    in
      header <> branch <> name' <> "\n" <> andMore

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
-- | into a target grove that already has an edge `A`
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
-- | It `insert`s every edges in a grove one by one into another grove.
merge :: forall a. Ord a => Grove a -> Grove a -> Grove a
merge old (Grove new) = foldrWithIndex insert old new

-- | Merging two groves makes another grove, yielding a semigroup algebra.
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

-- | Make a grove from a foldable, containing a sequence of edges.
-- |
-- | For example, `fromFoldable [1, 2, 3]` produces a linear grove:
-- |
-- | ```
-- | ∘─1─∘
-- |     └─2─∘
-- |         └─3─∘
-- | ```
fromFoldable :: forall f a. Foldable f => f a -> Maybe (Grove a)
fromFoldable xs = foldr step Nothing xs
  where
  step x Nothing = Just (singleton x)
  step x (Just g) = Just (cons x g)
