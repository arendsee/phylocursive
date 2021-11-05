{-# LANGUAGE FlexibleContexts #-}

module Algorithms (knight, findAll, elderAge, elderAgeNaive) where

import qualified Data.Set as S
import qualified Data.Bits as B

-- What is the minimum number of moves needed for a chess knight to move from A to B
knight :: String -> String -> Int
knight a0 b0
  | not (onBoard a0) || not (onBoard b0) = error "Invalid positions"
  | otherwise = knight' 0 S.empty (S.singleton b0) where
  
  knight' i s xs
    | S.member a0 xs = i
    | otherwise =
      let s' = S.union s xs
          xs' = S.difference (S.fold (\a b -> foldr S.insert b (moves a)) S.empty xs) s'
      in knight' (i+1) s' xs'

  moves :: String -> [String]
  moves [x, y]
    = filter onBoard
    [ [succ (succ x), succ y]
    , [succ (succ x), pred y]
    , [pred (pred x), succ y]
    , [pred (pred x), pred y]
    , [succ x, succ (succ y)]
    , [succ x, pred (pred y)]
    , [pred x, succ (succ y)]
    , [pred x, pred (pred y)]
    ]
  moves _ = []

  onBoard :: String -> Bool
  onBoard [x, y] = x >= 'a' && x <= 'h' && y >= '1' && y <= '8'
  onBoard _ = False



-- Find all numbers that sum to nSum, have nDigits digits, and where digits are ordered
findAll :: Int -> Int -> (Int,Maybe Int,Maybe Int)
findAll nSum nDigits = (total, smallest, largest) where
  xs = findAll' 9 nDigits nSum
  total = S.size xs
  smallest = fst <$> S.minView xs
  largest = fst <$> S.maxView xs
  
  findAll' :: Int -> Int -> Int -> S.Set Int
  findAll' maxDigit nDigits remaining
    | nDigits == 1 && remaining <= maxDigit && remaining > 0 = S.singleton remaining
    | nDigits > 1 && remaining > 0 =
      S.unions [S.map (\j -> i + 10 * j) (findAll' i (nDigits - 1) (remaining - i)) | i <- [1..maxDigit], i <= remaining]
    | otherwise = S.empty



elderAgeNaive :: Integer -> Integer -> Integer -> Integer -> Integer
elderAgeNaive m n l t = mod (sum [max 0 (B.xor m' n' - l) | m' <- [0..m-1],  n' <- [0..n-1]]) t


{-
  | 0 1 | 2 3 | 4
--|--------------
0 |     | x x |  
1 |     | x x |  
--|--------------
2 | x x |     | x
3 | x x |     | x
--|--------------
4 |     | x x |  

-}


-- define an operator of integer division
infixl 7 //
(//) = div

infixl 7 %
(%) = mod

elderAge :: Integer -> Integer -> Integer -> Integer -> Integer
elderAge m n l t = mod (f m n 1) t where 
  f :: Integer -> Integer -> Integer -> Integer
  f 0 0 _ = 0
  f mr nr i = bitAge i + f (B.rotateR mr 1) (B.rotateR nr 1) (i*2)


{-
     v------- m -----------v
                           
 .>  +-----------------+---+
 |   |                 |   |
 |   |   A             | B |
n|   |                 |   |
 |   |                 |---|
 |   |                 | C | r_n
 '>  +---------------------+
                        r_m
-}

  -- This approach is almost there (apart from the missing nC definition), but
  -- it doesn't account for l. I don't see a clear way to extend to algorithm
  -- to account for l, so I may need a different approach.
  bitAge :: Integer -> Integer
  bitAge d = 
    let r_m = m % (2 * d)
        r_n = n % (2 * d) 
        nA = n * (m - r_m) // 2 
        nB = r_m * (n - r_n) // 2
        nC = 0
    in (nA + nB + nC) * d
