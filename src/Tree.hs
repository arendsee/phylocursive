{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Tree
  ( TreeF(..)
  , treeC
  , treeD
  -- * cata examples
  , tips
  , totalBranchLength
  , maxDepth
  , newick
  -- * ana examples
  , binaryUltrametric
  -- * para examples
  , maxNodeChildren
  ) where

import Data.Text.Prettyprint.Doc
import Schema

data TreeF a b c r = NodeF a [(b, r)] | LeafF c
  deriving(Show, Ord, Eq, Functor)

treeA, treeB, treeC, treeD :: Term (TreeF String Int String)
treeA = In (NodeF "n1" [(12, In (LeafF "A")), (13, In (LeafF "B"))])
treeB = In (LeafF "C")
treeC = In (NodeF "n2" [(5, treeA), (16, treeB)])
treeD = In (NodeF "n8" [(6, treeC), (1, ana binaryUltrametric ["n3", "n4", "n5"]), (2, ana binaryUltrametric ["n6", "n7"])])


-- helper functions -----------------------------------------------------------

isLeaf :: TreeF a b c r -> Bool
isLeaf (LeafF _) = True
isLeaf _ = False

isNode :: TreeF a b c r -> Bool
isNode = not . isLeaf


-- cata -----------------------------------------------------------------------

tips :: Algebra (TreeF a b c) [c]
tips (NodeF _ xs) = (concat . map snd) xs
tips (LeafF x) = [x]

totalBranchLength :: Num b => Algebra (TreeF a b c) b
totalBranchLength (LeafF _) = 0
totalBranchLength (NodeF _ kids) = sum (map fst kids ++ map snd kids)

-- like tuple but doesn't create space
tuple' :: [Doc ann] -> Doc ann
tuple' = encloseSep "(" ")" ","

newick :: (Pretty a, Pretty b, Pretty c) => Algebra (TreeF a b c) (Doc ann)
newick (LeafF x) = pretty x
newick (NodeF x kids) = (tuple' [x <> ":" <> pretty b | (b, x) <- kids]) <> pretty x

maxDepth :: (Num b, Ord b) => Algebra (TreeF a b c) b
maxDepth (LeafF _) = 0
maxDepth (NodeF _ kids) = maximum [b + x | (b, x) <- kids]


-- ana ------------------------------------------------------------------------

binaryUltrametric :: CoAlgebra (TreeF String Int String) [String]
binaryUltrametric [] = NodeF "" []
binaryUltrametric [x] = LeafF x
binaryUltrametric xs =
  let (lhs, rhs) = splitAt (div (length xs) 2) xs
  in NodeF "" [(1, lhs), (1, rhs)]


-- para -----------------------------------------------------------------------

-- find the maximum number of non-leaf children of a single node
maxNodeChildren :: RAlgebra' (TreeF a b c) Int
maxNodeChildren (In (NodeF _ kids')) (NodeF _ kids) = maximum (length (filter (isNode . out . snd) kids') : map snd kids)
maxNodeChildren _ _ = 0


-- This particular case does save much over the explicit recursion case:

data Tree a b c = Node a [(b, Tree a b c)] | Leaf c

isLeaf' :: Tree a b c -> Bool
isLeaf' (Leaf _) = True
isLeaf' _ = False

isNode' :: Tree a b c -> Bool
isNode' = not . isLeaf'

maxNodeChildren' :: Tree a b c -> Int
maxNodeChildren' (Node _ kids) = maximum (length (filter (isNode' . snd) kids) : map (maxNodeChildren' . snd) kids)
maxNodeChildren' _ = 0
