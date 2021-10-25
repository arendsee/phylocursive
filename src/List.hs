{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module List
  ( ListF
  , toListF
  , List
  , toList
  , sumList
  , sumList'
  , runs
  , runs'
  ) where

import Schema

data ListF a r = NilF | ConsF a r
  deriving (Show, Ord, Eq, Functor)

toListF :: [a] -> Term (ListF a)
toListF [] = In NilF
toListF (x:xs) = In (ConsF x (toListF xs))

data List a = Nil | Cons a (List a)

toList :: [a] -> List a
toList [] = Nil
toList (x:xs) = Cons x (toList xs)

-- cata

sumList :: Num a => Algebra (ListF a) a
sumList NilF = 0 -- dubious, perhaps
sumList (ConsF x r) = x + r

sumList' :: Num a => List a -> a
sumList' Nil = 0
sumList' (Cons x rs) = x + sumList' rs



-- para

runs :: Eq a => RAlgebra' (ListF a) [Int]
runs _ NilF = []
runs (In (ConsF x (In NilF))) _ = [1]
runs (In (ConsF _ (In (ConsF y _)))) (ConsF x (r:rs))
  | x == y = r + 1 : rs
  | otherwise = 1 : (r : rs)


runs' :: Eq a => List a -> [Int]
runs' Nil = []
runs' (Cons x Nil) = [1]
runs' (Cons x rs@(Cons y xs)) = case runs' rs of
  (r:rs) -> if x == y
            then (r+1:rs)
            else (1:(r:rs))
