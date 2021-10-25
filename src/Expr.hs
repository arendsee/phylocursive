{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Expr
    ( Expr(..)
    , prettyExpr
    , call
    , identNames
    , countNodes
    , randExpr
    , Seed(..)
    ) where

import Schema
import qualified System.Random as Random
import Data.Text.Prettyprint.Doc

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]}) --add(10, 10)

-- cata

identNames :: Algebra Expr [String]
identNames (Unary s x) = s : x
identNames (Binary lhs s rhs) = s : lhs ++ rhs 
identNames (Call x xs) = x ++ concat xs
identNames (Paren x) = x
identNames (Literal _) = []
identNames (Ident s) = [s]

countNodes :: Algebra Expr Int
countNodes (Literal _) = 1
countNodes (Ident _) = 1
countNodes (Unary _ x) = 1 + x
countNodes (Binary lhs _ rhs) = 1 + lhs + rhs 
countNodes (Call x xs) = 1 + foldr (+) x xs
countNodes (Paren x) = 1 + x

prettyExpr :: Term Expr -> Doc ann
prettyExpr = cata go where
  go (Literal i) = "Literal<" <> parens (pretty i) <> ">"
  go (Ident name) = "Ident<" <> pretty name <> ">"
  go (Unary s x) = pretty s <+> x
  go (Binary lhs s rhs) = parens $ lhs <+> pretty s <+> rhs
  go (Call f args) = parens $ hsep (f : args)
  go (Paren x) = parens x

-- ana

data Seed = Seed {depth :: Int, rnd :: Random.StdGen}

-- generate random expressions, they are not guaranteed to be correctly typed
randExpr :: Seed -> Term Expr
randExpr = ana go where
  go seed@(Seed depth r0) =
    let (choice, r1) = Random.randomR (1 :: Int, if depth < 5 then 6 else 2) r0
    in case choice of
      1 -> let (int, _) = Random.randomR (0 :: Int, 127) r1
           in Literal int
      2 -> let (i, _) = Random.randomR (0 :: Int, 7) r1
           in Ident ["abcdefgh" !! i]
      3 -> let (i, r2) = Random.randomR (0 :: Int, 1) r1
           in Unary ["-!" !! i] (Seed (depth + 1) r2)
      4 -> let (i, r2) = Random.randomR (0 :: Int, 7) r1
           in let (lhs, rhs) = Random.split r2
              in Binary (Seed (depth + 1) lhs) ["abcdefgh" !! i] (Seed (depth + 1) rhs)
      5 -> let (lhs, rhs) = Random.split r1
           in Call (Seed (depth + 1) lhs) [Seed (depth + 1) rhs] 
      6 -> Paren (Seed (depth + 1) r1)
