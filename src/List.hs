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
  , nth1
  , nth2
  , nth3
  ) where

import Schema
import Data.Text.Prettyprint.Doc
import Data.List
import qualified Tree23 as T
import FingerTree ((|>), (<|))
import qualified FingerTree as F

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


-- Find kth largest elemenst in list xs of length N

-- -- This first solution is very succinct but runs in O(n log (n)) time. If I
-- -- know the list will beshort, say the number of students in my class, then
-- -- clarity trumps efficiency, and I would choose this solution.
nth1 :: Ord a => Int -> [a] -> a
nth1 n = last . take n . reverse . sort


-- The worst case perforance is O(N * k). The worst case is when the input list
-- is sorted small to large, this would require every element from (k+1)
-- onwards be inserted at the back of the list (O(k)).

nth2 :: Ord a => Int -> [a] -> Maybe a
nth2 _ [] = Nothing
nth2 n (x0:xs0) = f 1 [x0] xs0 where
  f i (x:_) []
    | i == n = Just x
    | otherwise = Nothing
  f i xs@(x:rs) (y:ys)
    | i < n = f (i + 1) (add y xs) ys
    | i == n && x > y = f i xs ys
    | i == n && x <= y = f i (add y rs) ys

add :: Ord a => a -> [a] -> [a] 
add x [] = [x]
add x ys@(y:rs)
  | x > y = y : add x rs
  | otherwise = x : ys 

-- The performance killer in nth2 is certainly the `add` step O(k). This can be
-- reduced to O(log(k)) if we swap the list for a tree. Inserts will cost
-- log_b(k), though for a finger tree, b can be large, making the insertions
-- effectively constant time.

nth3 :: (Show a, Ord a) => Int -> [a] -> Maybe a
nth3 _ [] = Nothing
nth3 k (x:xs) = T.minVal $ f 0 x (T.singleton x) xs where
  f _ _ ks [] = ks
  f size smallest ks (x:xs)
    | size < k = f (size + 1) (min smallest x) (T.insert x ks) xs
    | x < smallest = f size smallest ks xs
    | size == k = case T.deleteMin . T.insert x $ ks of
      (newSmallest, ks') -> f size newSmallest ks' xs
