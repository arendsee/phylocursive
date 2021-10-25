{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Schema
import Expr ()
import Tree ()
import List ()
import qualified List as L
import qualified Expr as E
import qualified Tree as T

import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text

someFunc :: IO ()
someFunc = do
  putStrLn . show $ cata E.identNames E.call
  putStrLn . show $ cata T.tips T.treeC 
  putStrLn . show $ cata T.totalBranchLength T.treeC 
  putStrLn . show $ cata T.maxDepth T.treeC
  putDoc $ cata T.newick T.treeC <> ";"
  putStrLn ""
  putDoc $ cata T.newick (ana T.binaryUltrametric ["A", "B", "C", "D", "E"])
  putStrLn ""
  putStrLn . show $ para' T.maxNodeChildren T.treeD
  putStrLn . show $ cata L.sumList (L.toListF [1,2,3,4])
  putStrLn . show $ L.sumList' (L.toList [1,2,3,4])
  putStrLn . show $ para' L.runs (L.toListF [1,2,2,2,3,4])
  putStrLn . show $ L.runs' (L.toList [1,2,2,2,3,4])
