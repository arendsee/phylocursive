{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings, FlexibleContexts #-}

module Nat
  ( Nat(..)
  , toNat
  , fromNat
  , factorial
  , fibonacci
  , ukp1
  , ukp2
  , ukp3
  , ukp4
  , unboundedKnapsack
  , change
  ) where

import Schema
import Data.Maybe
import Safe
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import qualified Data.List as DL
import qualified Data.List.Extra as DLE

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

-- The Unbounded Knapsack Problem. Input is a list of weights (e.g., coin
-- denominations) and a total desired size, sum of weights.
type UKP = [Int] -> Int -> Maybe [(Int, Int)]

-- Naive unbounded knapsack problem using explicit, unmemoized recursion. It
-- recursively tries each denomination and choose the one with the fewest
-- coins. Runs in exponential time.
ukp1 :: UKP
ukp1 ds i0 = countRuns . snd <$> (f i0) where
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
ukp2 :: UKP
ukp2 ds i0
  = countRuns 
  . concat
  . zipWith (\d n -> take n (repeat d)) ds
  <$> f 0 (take (length ds) (repeat 0))
  where
  f :: Int -- total number so far (starting at 0, counting up)
    -> [Int] -- counts for each denomination
    -> Maybe [Int] -- counts for each denomincation
  f total xs
    | total > i0 = Nothing
    | total == i0 = Just xs
    | otherwise = minimumBy length
        $ catMaybes [f (total + j) (inc k xs)
                    | (i, (j, maxN), k) <- zip3 xs mask [0..]
                    , j + total <= i0
                    , i + 1 <= maxN
                    ]
    where
    mask = [(,) small $ minimumDef (div i0 small)
             [div large small - 1
             | large <- ds, mod large small == 0, large > small
             ] | small <- ds]

    inc :: Int -> [Int] -> [Int]
    inc 0 (x:xs) = (x+1):xs
    inc i (x:xs) = x : inc (i-1) xs


type Memo a = Map a (a, [Int])

type UKPRem = [Int] -> Int -> (Int, [(Int, Int)])

ukp3 :: UKPRem
ukp3 ds0 i0 = (\(r, xs) -> (r, countRuns xs)) $ evalState (ukp i0) Map.empty where
  ds = DL.reverse . DL.sort $ ds0

  chk :: Int -> Int -> State (Memo Int) (Int, [Int])
  chk i j
    | k < 0 = return (j - i, [])
    | k == 0 = return (0, [j])
    | otherwise = do
      (r, xs) <- mem ukp k
      return (r, j:xs)
    where
      k = i - j

  ukp :: Int -> State (Memo Int) (Int, [Int])
  ukp i = fromJust . minimumBy criterion <$> mapM (chk i) ds

  -- What do you want?
  --   * The least leftover space?
  --   * The maximum number of items?
  --   * The minimum number of items?
  --   * Some balance between the two?
  --
  -- The coin problems wants the minimum number of items and a remainder of 0.
  -- But this a rather special case.
  criterion :: (Int, [Int]) -> (Int, Int)
  criterion (r, xs) = (r, length xs)

ukp4 :: UKPRem
ukp4 = unboundedKnapsack (\(x, xs) -> (x, length xs))



-- This is the final general form of the unbounded knapsack problem
unboundedKnapsack ::
  (Ord cost, Num weight, Ord weight)
  => ((weight, [weight]) -> cost) -- the remainder and the selected weights
  -> [weight] -- weights that are summed to try to hit the goal
  -> weight -- the goal
  -> (weight, [(weight, Int)])
unboundedKnapsack criterion weights goal = reshape $ evalState (ukp goal) Map.empty
  where
  -- Searching largest elements first with memoization is sufficient to avoid
  -- searching smaller denominations sum to larger denominations. That is, it
  -- avoids the problem solved by `mask` in ukp2.
  --
  -- This seems to make about a 2 fold difference for the 16384 case.
  ds = DL.reverse . DL.sort $ weights

  chk i j
    | k < 0 = return (j - i, [])
    | k == 0 = return (0, [j])
    | otherwise = do
      (r, xs) <- mem ukp k
      return (r, j:xs)
    where
      k = i - j

  ukp i = fromJust . minimumBy criterion <$> mapM (chk i) ds

  reshape (r, xs) = (r, countRuns xs)


mem :: Ord i => (i -> State (Map i o) o) -> i -> State (Map i o) o
mem f x = do
  m <- get
  case Map.lookup x m of
    (Just x') -> return x'
    Nothing -> do
      x' <- f x
      modify (Map.insert x x')
      return x'

countRuns :: (Ord a) => [a] -> [(a, Int)]
countRuns = map (\xs@(x:_) -> (x, length xs)) . DL.group

minimumBy :: Ord b => (a -> b) -> [a] -> Maybe a
minimumBy _ [] = Nothing
minimumBy _ [x] = Just x
minimumBy f (x:xs) = case minimumBy f xs of
  Nothing -> Just x
  (Just y) -> Just $ if f x < f y then x else y
