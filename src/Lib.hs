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

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text
import qualified System.Random as Random
import System.TimeIt

someFunc :: IO ()
someFunc = do
  rnd <- Random.newStdGen
  putStrLn . show $ cata E.identNames E.call
  putStrLn . show $ T.tips T.treeC 
  putStrLn . show $ T.totalBranchLength T.treeC 
  putStrLn . show $ T.maxDepth T.treeC
  putDoc $ T.newick T.treeC <> ";"
  putStrLn ""
  putDoc $ T.newick (T.binaryUltrametric ["A", "B", "C", "D", "E"])
  putStrLn ""
  putStrLn . show $ T.maxNodeChildren T.treeD
  putStrLn . show $ L.sumList (L.toListF [1,2,3,4])
  putStrLn . show $ L.sumList' (L.toList [1,2,3,4])
  putStrLn . show $ L.runs (L.toListF [1,2,2,2,3,4])
  putStrLn . show $ L.runs' (L.toList [1,2,2,2,3,4])
  putDoc . L.prettyListF . L.evens $ 12
  putStrLn ""
  putDoc . L.prettyListF . L.collatz $ 12
  putStrLn ""
  putDoc . L.prettyListF . L.collatz $ 10000
  putStrLn ""
  putDoc . T.newick . T.randTree $ T.Seed 0 rnd
  putStrLn ""
  putStrLn . show $ T.pathToLeaf "A" T.treeC
  putDoc . E.prettyExpr $ E.randExpr (E.Seed 0 rnd)
  putStrLn ""
  putDoc . E.prettyExpr $ E.randExpr (E.Seed 0 (fst $ Random.split rnd))
  putStrLn ""
  putStrLn . show $ N.factorial (N.toNat 6)
  putStrLn . show . N.fibonacci . N.toNat $ 1700
  putStrLn "Naive coin problem (exponential time)"
  timeIt . putStrLn . show $ N.ukp1 [1,5,10,25] 59
  putStrLn "Slightly less naive (still exponential time)"
  timeIt . putStrLn . show $ N.ukp2 [1,5,10,25] 59
  timeIt . putStrLn . show $ N.ukp2 [1,5,10,25] 149
  putStrLn "Memoized UKP"
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 4
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 11
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 40
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 149
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 256
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 512
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 1024  -- 0.01
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 2048  -- 0.03
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 4096  -- 0.09
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 8192  -- 0.27
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 16384 -- 1.06
  -- test with varying denominations
  putStrLn "Varying denominations"
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25] 8192
  timeIt . putStrLn . show $ N.ukp3 [1,5,10,25,
                                     271,273,91,67] 8192
  timeIt . putStrLn . show $ N.ukp3 [271,273,91,67,
                                     371,373,191,167,
                                     1113,1117,1111,1121,
                                     1313,1317,1311,1313,
                                     967,977,987,997,
                                     767,777,787,797,
                                     111111,111112] 181928
  timeIt . putStrLn . show $ N.ukp3 [111,131,171,244,91] 1111
  timeIt . putStrLn . show $ N.ukp3 [111,131,171] 1111
  timeIt . putStrLn . show $ N.ukp4 [111,131,171] 1111
