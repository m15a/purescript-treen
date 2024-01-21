-- | Treen represents a number of multi-way trees with named nodes.
module Treen.Data.Treen
  ( Treen
  , bundle
  , print
  , printWith
  ) where

import Prelude
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.String (trim) as S
import Treen.Data.Grove (Grove)
import Treen.Data.Grove (empty, fromFoldable, printWith) as G
import Treen.Data.Lineage (Lineage)
import Treen.Data.Lineage (toList) as L
import Treen.Data.Tileset (Tileset)
import Treen.Data.Tileset (tree) as TS

-- | Simply `Grove` with string-instantiated edge names, interpreted as
-- | node-named trees.
type Treen = Grove String

-- | Print a treen way prettier.
print :: Treen -> String
print = G.printWith TS.tree S.trim

-- | Print a treen with a custom tileset and a name formatter.
printWith :: Tileset -> (String -> String) -> Treen -> String
printWith = G.printWith

-- | Make a treen by mergeing all the given lineages.
bundle :: forall f. Foldable f => f Lineage -> Treen
bundle ls = foldl step G.empty ls
  where
  step g l = case G.fromFoldable (L.toList l) of
    Nothing -> g
    Just l' -> g <> l'
