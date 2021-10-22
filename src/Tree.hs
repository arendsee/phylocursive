{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Tree
  ( TreeF(..)
  , treeC
  , tips
  , totalBranchLength
  , maxDepth
  ) where

import Schema

data TreeF a b c r = NodeF a [(b, r)] | LeafF c
  deriving(Show, Ord, Eq, Functor)

treeA, treeB, treeC :: Term (TreeF String Int String)
treeA = In (NodeF "n1" [(12, In (LeafF "A")), (13, In (LeafF "B"))])
treeB = In (LeafF "C")
treeC = In (NodeF "n2" [(5, treeA), (16, treeB)])

tips :: Algebra (TreeF a b c) [c]
tips (NodeF _ xs) = (concat . map snd) xs
tips (LeafF x) = [x]

totalBranchLength :: Num b => Algebra (TreeF a b c) b
totalBranchLength (LeafF _) = 0
totalBranchLength (NodeF _ kids) = sum (map fst kids ++ map snd kids)

maxDepth :: (Num b, Ord b) => Algebra (TreeF a b c) b
maxDepth (LeafF _) = 0
maxDepth (NodeF _ kids) = maximum [b + x | (b, x) <- kids]
