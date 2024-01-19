-- | Treen represents a number of multi-way trees with named nodes.
module Treen.Data.Treen
  ( Treen
  , bundle
  , print
  ) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.FoldableWithIndex (foldlWithIndex)
import Data.Map (size, toUnfoldable) as M
import Data.Maybe (Maybe(..))
import Data.String.Common (joinWith) as S
import Data.Tuple (Tuple(..))
import Treen.Data.Grove (Grove(..))
import Treen.Data.Grove (empty, fromFoldable) as G
import Treen.Data.Lineage (Lineage)
import Treen.Data.Lineage (toList) as L
import Treen.Util.Data.String (trimLastEndOfLine) as S

-- | Simply `Grove` with string-instantiated edge names, interpreted as
-- | node-named trees.
type Treen = Grove String

-- | Print a treen way prettier.
print :: Treen -> String
print (Grove m) =
  m # M.toUnfoldable
    # map print'
    # S.joinWith "\n"

data NodeType
  = Root
  | OlderChild
  | LastChild

print' :: Tuple String Treen -> String
print' = go Root "" >>> S.trimLastEndOfLine
  where
  go nodeType header (Tuple nodeName (Grove children)) =
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
      header <> branch <> nodeName <> "\n" <> andMore

-- | Make a treen by mergeing all the given lineages.
bundle :: forall f. Foldable f => f Lineage -> Treen
bundle ls = foldl step G.empty ls
  where
  step g l = case G.fromFoldable (L.toList l) of
    Nothing -> g
    Just l' -> g <> l'
