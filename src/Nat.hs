{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Nat
  ( Nat(..)
  , toNat
  , fromNat
  , factorial
  , fibonacci
  , ukp1
  , ukp2
  ) where

import Schema
import Data.Maybe
import Safe

data Nat a = Zero | Succ a
  deriving(Show, Ord, Eq, Functor)

toNat :: Integer -> Term Nat
toNat 0 = In Zero
toNat i = In (Succ (toNat (i-1))) 

fromNat :: Term Nat -> Integer
fromNat = cata go where
  go Zero = 0
  go (Succ x) = 1 + x

factorial :: Term Nat -> Integer
factorial = para' go where
  go _ Zero = 1
  go t (Succ x) = fromNat t * x

fibonacci :: Term Nat -> Integer
fibonacci = histo go where
  go Zero = 1
  go (Succ (Attr _ Zero)) = 1
  go (Succ (Attr x (Succ (Attr y _)))) = x + y

-- Naive unbounded knapsack problem using explicit, unmemoized recursion. It
-- recursively tries each denomination and choose the one with the fewest
-- coins. Runs in exponential time.
ukp1
  :: [Int] -- denominations
  -> Int   -- total
  -> Maybe [Int] -- trace
ukp1 ds i0 = snd <$> (f i0) where
  f :: Int -> Maybe (Int, [Int])
  f i | i < 0 = Nothing
      | i == 0 = Just (0, [])
      | otherwise = minFst $ catMaybes [f (i-j) >>= merge j | j <- ds, j <= i]
      where
        merge j (depth, trace) = Just (depth + 1, j : trace)

        minFst :: Ord a => [(a,b)] -> Maybe (a,b)
        minFst [] = Nothing
        minFst x0@(y0:_) = f y0 x0 where
          f y [] = Just y 
          f (a, x) ((b, y):rs)
            | b < a = f (b, y) rs
            | otherwise = f (a, x) rs

-- First let's replace the [Int] datatype with [(Int, Int)], which contains
-- counts of each denomination. This is one step towards collapsing paths that
-- differ only by order. Also it will simplify the process of pruning branches
-- that are guaranteed to be sub-optimal (such as any path with 5 or more
-- pennies).
ukp2 :: [Int] -> Int -> Maybe [Int]
ukp2 ds i0 = concat . zipWith (\d n -> take n (repeat d)) ds <$> f 0 (take (length ds) (repeat 0)) where
  f :: Int -- total number so far (starting at 0, counting up)
    -> [Int] -- counts for each denomination
    -> Maybe [Int] -- counts for each denomincation
  f total xs
    | total > i0 = Nothing
    | total == i0 = Just xs
    | otherwise = minimumBy length
        $ catMaybes [f (total + j) (inc xs k)
                    | (i, (j, maxN), k) <- zip3 xs mask [0..]
                    , j + total <= i0
                    , i + 1 <= maxN
                    ]
    where
    mask = [(,) small $ minimumDef (div i0 small)
             [div large small - 1
             | large <- ds, mod large small == 0, large > small
             ] | small <- ds]

    inc :: [Int] -> Int -> [Int]
    inc (x:xs) 0 = (x+1):xs
    inc (x:xs) i = x : inc xs (i-1)

minimumBy :: Ord b => (a -> b) -> [a] -> Maybe a
minimumBy _ [] = Nothing
minimumBy _ [x] = Just x
minimumBy f (x:xs) = case minimumBy f xs of
  Nothing -> Just x
  (Just y) -> Just $ if f x < f y then x else y
