{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Lib
    ( someFunc
    ) where

import Schema
import Expr ()
import Tree ()
import qualified Expr as E
import qualified Tree as T

someFunc :: IO ()
someFunc = do
  putStrLn . show $ cata E.identNames E.call
  putStrLn . show $ cata T.tips T.treeC 
  putStrLn . show $ cata T.totalBranchLength T.treeC 
  putStrLn . show $ cata T.maxDepth T.treeC
