{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Tree
  ( TreeF(..)
  , treeC
  , tips
  , totalBranchLength
  , maxDepth
  , newick
  , binaryUltrametric
  ) where

import Data.Text.Prettyprint.Doc
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

-- like tuple but doesn't create space
tuple' :: [Doc ann] -> Doc ann
tuple' = encloseSep "(" ")" ","

newick :: (Pretty a, Pretty b, Pretty c) => Algebra (TreeF a b c) (Doc ann)
newick (LeafF x) = pretty x
newick (NodeF x kids) = (tuple' [x <> ":" <> pretty b | (b, x) <- kids]) <> pretty x

maxDepth :: (Num b, Ord b) => Algebra (TreeF a b c) b
maxDepth (LeafF _) = 0
maxDepth (NodeF _ kids) = maximum [b + x | (b, x) <- kids]

binaryUltrametric :: Coalgebra (TreeF String Int String) [String]
binaryUltrametric [] = NodeF "" []
binaryUltrametric [x] = LeafF x
binaryUltrametric xs =
  let (lhs, rhs) = splitAt (div (length xs) 2) xs
  in NodeF "" [(1, lhs), (1, rhs)]
