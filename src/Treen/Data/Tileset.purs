-- | Tilesets for pretty-printing trees.
-- |
-- | A tileset contains the following *tiles*:
-- |
-- |   1. `lastChildBranch`: prints a branch growing from a node to its youngest child,
-- |      so that the branch is the last one.
-- |   2. `lastChildHeader`: prints an indent when a node is the youngest child of its
-- |      parent, so that there will be no more branching.
-- |   3. `olderChildBranch`: prints a branch growing from a node to its older child,
-- |      so that the branch is more branching to the younger children.
-- |   4. `olderChildHeader`: prints an indent when a node is not the youngest of its
-- |      parent, so that there will be more branching.
-- |
-- | For example, You see the following tree,
-- |
-- | ```
-- | A
-- | ├── B
-- | │   └── C
-- | └── D
-- |     └── E
-- | ```
-- |
-- | where
-- |
-- | ```
-- | A
-- | ├── B
-- | ```
-- |
-- | `B` is not the youngest child of `A`, so here you see the branch tile 3 `├── `,
-- | 
-- | ```
-- | │   └── C
-- | ```
-- |
-- | `B` was not the youngest child of `A`, so here you see the header tile 4 `│   `,
-- | and `C` is the youngest child of `B`, so you also see the tile 1 `└── `,
-- |
-- | ```
-- | └── D
-- | ```
-- |
-- | `D` is the youngest child of `A`, so here you see again the branch tile 1 `└── `,
-- |
-- | ```
-- |     └── E
-- | ```
-- |
-- | `D` was the youngest child of `A`, so here you see the header tile 2 `    `, and
-- | `E` is the youngest child of `D`, so you finally see the branch tile 1 `└── `.
-- |
-- | Moreover, a tileset contains the following pre/postfixes for node names:
-- |
-- |    1. `branchPrefix`: prefixes a node's name if it is non-terminal.
-- |    2. `branchPostfix`: postfixes a non-terminal node.
-- |    3. `leafPrefix`: prefixes a terminal node, i.e., a leaf.
-- |    4. `leafPostfix`: postfixes a leaf.
-- |
-- | For example, You see the following tree,
-- |
-- | ```
-- | <foo>
-- | └── [bar]
-- | ```
-- |
-- | where `foo` is non-terminal, surrounded by its prefix `<` and postfix `>`,
-- | and `bar` is a leaf, surrounded by its prefix `[` and postfix `]`.
module Treen.Data.Tileset
  ( Tileset
  , tree
  , colon
  ) where

type Tileset =
  { lastChildBranch :: String
  , lastChildHeader :: String
  , olderChildBranch :: String
  , olderChildHeader :: String
  , branchPrefix :: String
  , branchPostfix :: String
  , leafPrefix :: String
  , leafPostfix :: String
  }

-- | Plain tree tileset.
tree :: Tileset
tree =
  { lastChildBranch: "└── "
  , lastChildHeader: "    "
  , olderChildBranch: "├── "
  , olderChildHeader: "│   "
  , branchPrefix: ""
  , branchPostfix: ""
  , leafPrefix: ""
  , leafPostfix: ""
  }

-- | YAML-ish tileset.
colon :: Tileset
colon =
  { lastChildBranch: "  "
  , lastChildHeader: "  "
  , olderChildBranch: "  "
  , olderChildHeader: "  "
  , branchPrefix: ""
  , branchPostfix: ":"
  , leafPrefix: ""
  , leafPostfix: ""
  }
