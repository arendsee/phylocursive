{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Schema
import Expr ()
import Tree ()
import List ()
import Nat  ()
import qualified List as L
import qualified Expr as E
import qualified Nat  as N
import qualified Tree as T
import qualified Tree23 as T23
import qualified FingerTree as FT
import qualified Algorithms as A
import qualified ALaCarte as Ala

import qualified Data.List as DL
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified System.Random as Random
import System.TimeIt
import Control.Monad (replicateM)

trueOrElse :: Eq a => a -> a -> Maybe (a, a)
trueOrElse a b
  | a == b = Nothing
  | otherwise = Just (a, b)

someFunc :: IO ()
someFunc = do
  -- rnd <- Random.newStdGen
  -- putStrLn . show $ cata E.identNames E.call
  -- putStrLn . show $ T.tips T.treeC
  -- putStrLn . show $ T.totalBranchLength T.treeC
  -- putStrLn . show $ T.maxDepth T.treeC
  -- putDoc $ T.newick T.treeC <> ";"
  -- putStrLn ""
  -- putDoc $ T.newick (T.binaryUltrametric ["A", "B", "C", "D", "E"])
  -- putStrLn ""
  -- putStrLn . show $ T.maxNodeChildren T.treeD
  -- putStrLn . show $ L.sumList (L.toListF [1,2,3,4])
  -- putStrLn . show $ L.sumList' (L.toList [1,2,3,4])
  -- putStrLn . show $ L.runs (L.toListF [1,2,2,2,3,4])
  -- putStrLn . show $ L.runs' (L.toList [1,2,2,2,3,4])
  -- putDoc . L.prettyListF . L.evens $ 12
  -- putStrLn ""
  -- putDoc . L.prettyListF . L.collatz $ 12
  -- putStrLn ""
  -- putDoc . L.prettyListF . L.collatz $ 10000
  -- putStrLn ""
  -- putDoc . T.newick . T.randTree $ T.Seed 0 rnd
  -- putStrLn ""
  -- putStrLn . show $ T.pathToLeaf "A" T.treeC
  -- putDoc . E.prettyExpr $ E.randExpr (E.Seed 0 rnd)
  -- putStrLn ""
  -- putDoc . E.prettyExpr $ E.randExpr (E.Seed 0 (fst $ Random.split rnd))
  -- putStrLn ""
  -- putStrLn . show $ N.factorial (N.toNat 6)
  -- putStrLn . show . N.fibonacci . N.toNat $ 1700
  -- putStrLn "Naive coin problem (exponential time)"
  -- timeIt . putStrLn . show $ N.ukp1 [1,5,10,25] 59
  -- putStrLn "Slightly less naive (still exponential time)"
  -- timeIt . putStrLn . show $ N.ukp2 [1,5,10,25] 59
  -- timeIt . putStrLn . show $ N.ukp2 [1,5,10,25] 149
  -- putStrLn "Memoized UKP"
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 4
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 11
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 40
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 149
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 256
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 512
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 1024  -- 0.01
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 2048  -- 0.03
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 4096  -- 0.09
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 8192  -- 0.27
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 16384 -- 1.06
  -- -- test with varying denominations
  -- putStrLn "Varying denominations"
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 8192
  -- timeIt . putStrLn . show $ N.ukp3 [1,5,10,25,
  --                                    271,273,91,67] 8192
  -- timeIt . putStrLn . show $ N.ukp3 [271,273,91,67,
  --                                    371,373,191,167,
  --                                    1113,1117,1111,1121,
  --                                    1313,1317,1311,1313,
  --                                    967,977,987,997,
  --                                    767,777,787,797,
  --                                    111111,111112] 181928
  -- timeIt . putStrLn . show $ N.ukp3 [111,131,171,244,91] 1111
  -- timeIt . putStrLn . show $ N.ukp3 [111,131,171] 1111
  -- timeIt . putStrLn . show $ N.ukp4 [111,131,171] 1111

  -- putStrLn "-------------------------------"
  -- putStrLn . show $ L.nth2 1 [1,3,2]
  -- putStrLn . show $ L.nth2 2 [1,3,2]
  -- putStrLn . show $ L.nth2 5 [1,3,2]
  -- putStrLn . show $ L.nth2 2 [5,1,3,3,1]
  -- putStrLn . show $ L.nth2 5 [5,4,3,2,1,6,7,8,9]

  -- let n = 5000000
  --     orderedList = [1..n]
  --     randomInt = Random.getStdRandom (Random.randomR (1, n))
  -- randomList <- replicateM n (randomInt :: IO Int)

  -- putStrLn "Ordered List"
  -- -- force evaluation
  -- timeIt . putStrLn . show . last $ orderedList
  -- putStrLn "k=1"
  -- timeIt . putStrLn . show $ L.nth1 1 orderedList
  -- timeIt . putStrLn . show $ L.nth2 1 orderedList
  -- timeIt . putStrLn . show $ L.nth3 1 orderedList
  -- putStrLn "k=10"
  -- timeIt . putStrLn . show $ L.nth1 10 orderedList
  -- timeIt . putStrLn . show $ L.nth2 10 orderedList
  -- timeIt . putStrLn . show $ L.nth3 10 orderedList
  -- putStrLn "k=100"
  -- timeIt . putStrLn . show $ L.nth1 100 orderedList
  -- timeIt . putStrLn . show $ L.nth2 100 orderedList
  -- timeIt . putStrLn . show $ L.nth3 100 orderedList

  -- putStrLn "Random List"
  -- -- force evaluation
  -- timeIt . putStrLn . show . last $ randomList
  -- putStrLn "k=1"
  -- timeIt . putStrLn . show $ L.nth1 1 randomList
  -- timeIt . putStrLn . show $ L.nth2 1 randomList
  -- timeIt . putStrLn . show $ L.nth3 1 randomList
  -- putStrLn "k=10"
  -- timeIt . putStrLn . show $ L.nth2 10 randomList
  -- timeIt . putStrLn . show $ L.nth3 10 randomList
  -- putStrLn "k=100"
  -- timeIt . putStrLn . show $ L.nth2 100 randomList
  -- timeIt . putStrLn . show $ L.nth3 100 randomList
  -- putStrLn "k=1000"
  -- timeIt . putStrLn . show $ L.nth2 1000 randomList
  -- timeIt . putStrLn . show $ L.nth3 1000 randomList
  -- putStrLn "k=10000"
  -- timeIt . putStrLn . show $ L.nth2 10000 randomList
  -- timeIt . putStrLn . show $ L.nth3 10000 randomList
  -- putStrLn "k=100000"
  -- timeIt . putStrLn . show $ L.nth3 100000 randomList
  -- putStrLn "k=1000000"
  -- timeIt . putStrLn . show $ L.nth3 1000000 randomList

  -- putStrLn . show $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11]
  -- putStrLn . show $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11,12]
  -- putStrLn . show $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11,12,13]
  -- putStrLn . show $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11,12,13,14]
  -- putStrLn . show $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11,12,13,14,15]
  -- putStrLn . show $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11,12,13,14,15,16]
  -- putStrLn . show . T23.lookupNearest 11 $ T23.fromList [1,4,2,3,5,6,7,8,9,10,13]
  --
  -- putStrLn . show $ T23.fromList [1,1,1,1,1,1,1,1,1]
  -- putStrLn . show $ T23.fromList (DL.reverse [1..17])

  -- putStrLn . show $ T23.fromList (DL.reverse [1..8] ++ [1..8])
  -- putStrLn . show . T23.deleteMin $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11]
  -- putStrLn . show . T23.deleteMin . T23.deleteMin $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11]
  -- putStrLn . show . T23.deleteMin . T23.deleteMin . T23.deleteMin $ T23.fromList [1,4,2,3,5,6,7,8,9,10,11]

  -- let n = 1000000
  --     orderedList = [1..n]
  --     randomInt = Random.getStdRandom (Random.randomR (1, n))
  -- randomList <- replicateM n (randomInt :: IO Int)
  -- putStrLn . show . last $ randomList -- force evaluation
  --
  -- putStrLn "building a tree"
  -- timeIt . putStrLn . show . T23.lookupNearest 100 $ T23.fromList randomList
  -- putStrLn "building and sorting a list"
  -- timeIt . putStrLn . show . last . DL.sort $ randomList

  -- putStrLn . show $ FT.toTree [1,2,3]
  -- putStrLn . show $ FT.toTree [1,2,3,4,5,6,7]
  --
  -- putStrLn . show . FT.headMay $ FT.toTree [1,2,3,4,5,6,7]
  -- putStrLn . show . FT.tailMay $ FT.toTree [1,2,3,4,5,6,7]
  -- putStrLn . show . FT.initMay $ FT.toTree [1,2,3,4,5,6,7]
  -- putStrLn . show . FT.lastMay $ FT.toTree [1,2,3,4,5,6,7]

  -- putStrLn . show $ A.knight "a3" "c4"
  -- putStrLn . show $ A.knight "a3" "b3"
  -- putStrLn . show $ A.knight "a1" "h8"
  -- putStrLn . show $ A.knight "a1" "h8"
  --
  -- putStrLn . show $ A.findAll 10 3

  -- -- square cases
  -- putStrLn . show $ trueOrElse (A.elderAge 1 1 0 100) 0
  -- putStrLn . show $ trueOrElse (A.elderAge 2 2 0 100) 2
  -- putStrLn . show $ trueOrElse (A.elderAge 4 4 0 100) 24 -- 4*1 + 4*2 + 4*3 = 24
  -- putStrLn . show $ trueOrElse (A.elderAge 4 4 1 100) 12 -- 4*0 + 4*1 + 4*2 = 12
  -- putStrLn . show $ trueOrElse (A.elderAge 4 4 2 100) 4 -- 4*0 + 4*0 + 4*1 =  4
  -- putStrLn . show $ trueOrElse (A.elderAge 4 4 3 100) 0 -- 4*0 + 4*0 + 4*0 =  0
  -- putStrLn . show $ trueOrElse (A.elderAge 4 4 4 100) 0 -- 4*0 + 4*0 + 4*0 =  0
  --
  -- -- mod case
  -- putStrLn . show $ trueOrElse (A.elderAge 4 4 0 10) 4 -- 4
  --
  -- -- big mod square (test for performance)
  -- putStrLn . show $ A.elderAge (2^11620) (2^11620) 0 173
  --
  -- -- perfect rectangle
  -- putStrLn . show $ trueOrElse (A.elderAge 2 4 0 100) 12  -- 2*0 + 2*1 + 2*2 + 2*3 = 12
  -- putStrLn . show $ trueOrElse (A.elderAge 2 8 0 900) 56  -- 2*0 + 2*1 + 2*2 + 2*3 = 56
  -- putStrLn . show $ trueOrElse (A.elderAge 4 32 0 9999) 1984  -- 2*0 + 2*1 + 2*2 + 2*3 = 56
  --
  -- -- irregular
  -- putStrLn . show $ trueOrElse (A.elderAge 2 3 0 100) 7 -- (2*0 + 2*1) + (0+1, 1+1)
  --
  -- -- irregular tests
  -- putStrLn "irregular tests"
  -- putStrLn . show $ trueOrElse (A.elderAge 4 3 1 100) (A.elderAgeNaive 4 3 1 100)
  -- putStrLn . show $ trueOrElse (A.elderAge 4 3 0 100) (A.elderAgeNaive 4 3 0 100)
  -- putStrLn . show $ trueOrElse (A.elderAge 8 5 0 100) (A.elderAgeNaive 8 5 0 100)
  -- putStrLn . show $ trueOrElse (A.elderAge 8 5 1 100) (A.elderAgeNaive 8 5 1 100)
  -- putStrLn . show $ trueOrElse (A.elderAge 8 6 0 100) (A.elderAgeNaive 8 6 0 100)
  -- putStrLn . show $ trueOrElse (A.elderAge 9 1 0 100) (A.elderAgeNaive 9 1 0 100)
  -- putStrLn . show $ trueOrElse (A.elderAge 1 9 0 100) (A.elderAgeNaive 1 9 0 100)
  --
  --
  -- putStrLn "more irregular"
  -- putStrLn . show $ trueOrElse (A.elderAge 5 9 0 9999) (A.elderAgeNaive 5 9 0 9999)
  -- putStrLn . show $ trueOrElse (A.elderAge 5 32 0 999999999) (A.elderAgeNaive 5 32 0 999999999)
  -- putStrLn . show $ trueOrElse (A.elderAge 5 13 0 999999999) (A.elderAgeNaive 5 13 0 999999999)
  -- putStrLn . show $ trueOrElse (A.elderAge 5 8 0 999999999) (A.elderAgeNaive 5 8 0 999999999)
  -- putStrLn "3X3"
  -- putStrLn . show $ trueOrElse (A.elderAge 3 3 0 999999999) (A.elderAgeNaive 3 3 0 999999999)
  -- putStrLn "5X5"
  -- putStrLn . show $ trueOrElse (A.elderAge 5 5 0 999999999) (A.elderAgeNaive 5 5 0 999999999)
  -- putStrLn "5X4"
  -- putStrLn . show $ trueOrElse (A.elderAge 5 4 0 999999999) (A.elderAgeNaive 5 4 0 999999999)
  -- putStrLn "5X1"
  -- putStrLn . show $ trueOrElse (A.elderAge 5 1 0 999999999) (A.elderAgeNaive 5 1 0 999999999)
  -- putStrLn . show $ trueOrElse (A.elderAge 5 45 0 999999999) (A.elderAgeNaive 5 45 0 999999999)
  --
  -- putStrLn . show $ trueOrElse (A.elderAge 1 4  3 999999999) (A.elderAgeNaive 1 4 3 999999999)
  --
  -- -- codewars tests
  -- putStrLn "codewars tests"
  -- putStrLn . show $ trueOrElse (A.elderAge 8 5 1 100) 5
  -- putStrLn . show $ trueOrElse (A.elderAge 8 8 0 1000007) 224
  -- putStrLn . show $ trueOrElse (A.elderAge 25 31 0 1000007) 11925
  -- putStrLn . show $ trueOrElse (A.elderAge 5 45 3 1000007) 4323
  -- putStrLn . show $ trueOrElse (A.elderAge 31 39 7 2345) 1586
  -- putStrLn . show $ trueOrElse (A.elderAge 545 435 342 1000007) 808451
  -- putStrLn . show $ trueOrElse (A.elderAge 28827050410 35165045587 7109602 13719506) 5456283
  
  putStrLn . show $ Ala.eval (Ala.expr2)
  putStrLn . show $ Ala.pretty (Ala.expr2)
