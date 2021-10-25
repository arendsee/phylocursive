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
  , pathToLeaf
  -- * ana examples
  , binaryUltrametric
  -- * para examples
  , maxNodeChildren
  -- * futu examples
  , Seed(..)
  , randTree
  ) where

import Data.Text.Prettyprint.Doc
import Schema
import Data.Maybe
import qualified System.Random as Random

data TreeF a b c r = NodeF a [(b, r)] | LeafF c
  deriving(Show, Ord, Eq, Functor)

treeA, treeB, treeC, treeD :: Term (TreeF String Int String)
treeA = In (NodeF "n1" [(12, In (LeafF "A")), (13, In (LeafF "B"))])
treeB = In (LeafF "C")
treeC = In (NodeF "n2" [(5, treeA), (16, treeB)])
treeD = In (NodeF "n8" [(6, treeC), (1, binaryUltrametric ["n3", "n4", "n5"]), (2, binaryUltrametric ["n6", "n7"])])


-- helper functions -----------------------------------------------------------

isLeaf :: TreeF a b c r -> Bool
isLeaf (LeafF _) = True
isLeaf _ = False

isNode :: TreeF a b c r -> Bool
isNode = not . isLeaf

-- like tuple but doesn't create space
tuple' :: [Doc ann] -> Doc ann
tuple' = encloseSep "(" ")" ","

-- cata -----------------------------------------------------------------------


tips :: Term (TreeF a b c) -> [c]
tips = cata go where
  go :: Algebra (TreeF a b c) [c]
  go (NodeF _ xs) = (concat . map snd) xs
  go (LeafF x) = [x]

totalBranchLength :: Num b => Term (TreeF a b c) -> b
totalBranchLength = cata go where
  go :: Num b => Algebra (TreeF a b c) b
  go (LeafF _) = 0
  go (NodeF _ kids) = sum (map fst kids ++ map snd kids)

newick :: (Pretty a, Pretty b, Pretty c) => Term (TreeF a b c) -> Doc ann
newick = cata go where
  go (LeafF x) = pretty x
  go (NodeF x kids) = (tuple' [x <> ":" <> pretty b | (b, x) <- kids]) <> pretty x

maxDepth :: (Num b, Ord b) => Term (TreeF a b c) -> b
maxDepth = cata go where
  go :: (Num b, Ord b) => Algebra (TreeF a b c) b
  go (LeafF _) = 0
  go (NodeF _ kids) = maximum [b + x | (b, x) <- kids]

pathToLeaf :: (Eq c) => c -> Term (TreeF a b c) -> Maybe [a]
pathToLeaf searchLeaf = cata go where
  go (LeafF foundLeaf)
    | searchLeaf == foundLeaf = Just []
    | otherwise = Nothing
  go (NodeF a kids) = fmap ((:) a) $ listToMaybe [xs | (_, Just xs) <- kids]

-- ana ------------------------------------------------------------------------

binaryUltrametric :: [String] -> Term (TreeF String Int String)
binaryUltrametric = ana go where
  go :: CoAlgebra (TreeF String Int String) [String]
  go [] = NodeF "" []
  go [x] = LeafF x
  go xs =
    let (lhs, rhs) = splitAt (div (length xs) 2) xs
    in NodeF "" [(1, lhs), (1, rhs)]


-- problem: grow a random tree with leaf probability increasing with depth)

data Seed = Seed
  { depth :: Double
  , rng :: Random.StdGen
  }

randTree :: Seed -> Term (TreeF String Int String)
randTree = ana go where
  go :: CoAlgebra (TreeF String Int String) Seed
  go (Seed depth rng)
    -- make a leaf
    | p < depth / (depth + 3) = LeafF "."
    -- branch
    | otherwise = NodeF "*" [(1, l), (1, r)]
    where
    -- generate a leaf probability
    (p, _) = Random.randomR (0 :: Double, 1) rng

    -- create right and left hand random number generators
    (rhs, lhs) = Random.split rng 

    -- generate right and left trees and incremented depth
    r = Seed (depth + 1) lhs
    l = Seed (depth + 1) rhs


-- para -----------------------------------------------------------------------

-- find the maximum number of non-leaf children of a single node
maxNodeChildren :: Term (TreeF a b c) -> Int
maxNodeChildren = para' go where
  go :: RAlgebra' (TreeF a b c) Int
  go (In (NodeF _ kids')) (NodeF _ kids) = maximum (length (filter (isNode . out . snd) kids') : map snd kids)
  go _ _ = 0



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


-- futu

-- randTree :: Seed -> Term (TreeF String Int String)
-- randTree = futu go where
--   go :: CVCoAlgebra (TreeF String Int String) Seed
--   go (Seed depth rng)
--     -- make a leaf
--     | p < depth / (depth + 3) = LeafF "."
--     -- branch
--     | otherwise = NodeF "*" [(1, l), (1, r)]
--     where
--     -- generate a leaf probability
--     (p, _) = Random.randomR (0 :: Double, 1) rng
--
--     -- create right and left hand random number generators
--     (rhs, lhs) = Random.split rng
--
--     -- generate right and left trees and incremented depth
--     r = Automatic (Seed (depth + 1) lhs)
--     l = Automatic (Seed (depth + 1) rhs)
