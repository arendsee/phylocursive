{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module List
  ( ListF
  , toListF
  , prettyListF
  , List
  , toList
  , sumList
  , sumList'
  , runs
  , runs'
  , evens
  , collatz
  ) where

import Schema
import Data.Text.Prettyprint.Doc

data ListF a r = NilF | ConsF a r
  deriving (Show, Ord, Eq, Functor)

toListF :: [a] -> Term (ListF a)
toListF [] = In NilF
toListF (x:xs) = In (ConsF x (toListF xs))

data List a = Nil | Cons a (List a)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

prettyListF :: Pretty a => Term (ListF a) -> Doc ann
prettyListF = cata go where
  go NilF = "Nil"
  go (ConsF x rs) = pretty x <+> ":" <+> rs 

-- cata

sumList :: Num a => Term (ListF a) -> a
sumList = cata go where
  go :: Num a => Algebra (ListF a) a
  go NilF = 0 -- dubious, perhaps
  go (ConsF x r) = x + r

sumList' :: Num a => List a -> a
sumList' Nil = 0
sumList' (Cons x rs) = x + sumList' rs

-- para


runs :: Eq a => Term (ListF a) -> [Int]
runs = para' go where
  go :: Eq a => RAlgebra' (ListF a) [Int]
  go _ NilF = []
  go (In (ConsF x (In NilF))) _ = [1]
  go (In (ConsF _ (In (ConsF y _)))) (ConsF x (r:rs))
     | x == y = r + 1 : rs
     | otherwise = 1 : (r : rs)


runs' :: Eq a => List a -> [Int]
runs' Nil = []
runs' (Cons x Nil) = [1]
runs' (Cons x rs@(Cons y xs)) = case runs' rs of
  (r:rs) -> if x == y
            then (r+1:rs)
            else (1:(r:rs))

-- ana

evens :: Integer -> Term (ListF Integer)
evens = ana go where
  go :: CoAlgebra (ListF Integer) Integer
  go 0 = NilF
  go n = ConsF (2 * n) (n - 1)

collatz :: Integer -> Term (ListF Integer)
collatz = ana go where
  go :: CoAlgebra (ListF Integer) Integer
  go 1 = NilF
  go x
    | mod x 2 == 1 = ConsF x (x * 3 + 1)
    | otherwise = ConsF x (div x 2)

-- apo
