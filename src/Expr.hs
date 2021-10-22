{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Expr
    ( Expr(..)
    , call
    , identNames
    , countNodes
    ) where

import Schema

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]}) --add(10, 10)

identNames :: Algebra Expr [String]
identNames (Index target idx) = target ++ idx
identNames (Unary s x) = s : x
identNames (Binary lhs s rhs) = s : lhs ++ rhs 
identNames (Call x xs) = x ++ concat xs
identNames (Paren x) = x
identNames (Literal _) = []
identNames (Ident s) = [s]

countNodes :: Algebra Expr Int
countNodes (Literal _) = 1
countNodes (Ident _) = 1
countNodes (Index i j) = 1 + i + j
countNodes (Unary _ x) = 1 + x
countNodes (Binary lhs _ rhs) = 1 + lhs + rhs 
countNodes (Call x xs) = 1 + foldr (+) x xs
countNodes (Paren x) = 1 + x
