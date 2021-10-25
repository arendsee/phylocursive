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
