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


-- define an operator of integer division
infixl 7 //
(//) = div

infixl 7 %
(%) = mod

{-

   +--------+---+
   |        |   |   Every rectangle is divided into 4 pieces. A the largest
   |   A    | B |   square that can fit in the input rectangle. The square
   |        |   |   can then be doubled downwards. Parts B, C and D are sub-
   +--------+---+   problems that are solved recursively.
   |   C    | D |
   +--------+---+

   {n_0 : 1}  {n_0 : 2, n_1 : 2} {n_0 : 4, n_1 : 8, n_2 : 4}  4 * f(k-1) + 2 * (2^(k-1))^2
   f(0)       f(1)                f(2)
   0          0 1                 0 1 2 3                     0 . . 2^k
              1 0                 1 0 3 2                     . . . .
                                  2 3 0 1                     . . . .
                                  3 2 1 0                     2^k . .
                                   
                              note the four submatrices:
                                 0 1   2 3
                                 1 0   3 2  This is two f(0)'s and two f(0)'s with 2 added everywhere.
                                            Why? We XOR over every binary bit. This leads to overlayed 
                                 2 3   0 1  checkerboards. f(2) is the sum of these two checkerboards:
                                 3 2   1 0       0 1 0 1    0 0 2 2
                                                 1 0 1 0    0 0 2 2
                                                 0 1 0 1    2 2 0 0
                                                 1 0 1 0    2 2 0 0
                                            In fact, it is the sum of infinite checkerboards that are 
                                            all 0 in this top-left square. Moving from f(k) to f(k+1)
                                            always duplicates f(k) and creates two new boxes corresponding
                                            to f(k) + 2^k. We can increase in size be doubling and summing
                                            counts.
  For example:

  f(0) = {n_0 : 1}
  f(1) = 2 * f(0) + 2 * (f(0) + 2^0)
       = 2 * {n_0 : 1} + 2 * {n_1 : 1} 
       = {n_0 : 2, n_1 : 2}
  f(2) = 2 * f(1) + 2 * (f(1) + 2^1)
       = {n_0 : 4, n_1 : 4, n_2 : 4, n_3 : 4}
  ...
  f(k) = 2 * f(k-1) + 2 * (f(k-1) + 2^k)

  All multiplication and sums here are, of course, over the maps.

  Now, is it coincidence that all the counts sum to 4? And will every map
  happen to sum to 2^k for all values in [0..k-1]? Why yes, how convenient.
  f(k) is repeated twice, so all old counts are doubled. Then it is repeats
  right and down twice with 2^k added. This number is larger than any possible
  number in f(k), so there can be no overlap between the two sets of matrices.
  And they all contain the same number of unique elements, so the counts are
  simply doubled every time.

  So we can factor out 2^k. The sum of the square then becomes: 2^k *
  sum([0..k-1]). This simplifies to 2^n(n+1)/2.


  The other operation we need is linear doubling of a square. This is needed to
  deal with pathological cases such as (n X 1) matrices which would otherwise
  run in linear time.
                             a
                2^{kn}                     a - 2^{kn}
          +-----------------------------------+---+
          |        .        .        .        |   |
  b = 2^n |        .   A    .        .        | B |
          |        .        .        .        |   |
          +--------+--------+--------+--------+---+

    Given integers k and n, if b=n^n then regions C and D can be ignored and
    region A can be doubled k times.

  0 1  -->   0 1  2 3  ------>  0 1 2 3  4 5 6 7
  1 0        1 0  3 2           1 0 3 2  5 4 7 6

  f(1)       f(1) + (f(1)+2)    f(2) + (f(2)+4) .... f(k-1) + (f(k-1)+2^(k-1)) 

  The same principle can be symmetrically applied downwards. Notice there are
  exactly two of each number from [0..7], or more generally from [0..2^k-1].
  More generally still, this lateral copying will maintain count, but apply
  them to double the numbers each time, thus:

  f(k)_1 = {n_0 : 2^k, n_1 : 2^k, ..., n_{k-1} : 2^k}
  f(k)_2 = {n_0 : 2^k, n_1 : 2^k, ..., n_{k-1} : 2^k, n_k : 2^k, ... n_{2k-1} : 2^k}

  This simplifies to: 2^k * sum([0..jk-1]) = 2^k (jk-1)(jk)/2
-}

elderAge :: Integer -> Integer -> Integer -> Integer -> Integer
elderAge m0 n0 l t = mod (f m0 n0 0) t where
  f :: Integer -> Integer -> Integer -> Integer
  f 0 _ _ = 0
  f _ 0 _ = 0
  f a b v
    | a < b = f b a v                                
    | a == nth && b == nth = nth * sq nth  -- square with sides of length 2^n 
    | b == nth = let k = floorLog2 (a // nth)             --   2^(n+k) a-2^(n+k)
                     knth = 2 ^ (n + k)                   -- +--------+---+
                 in   nth * sq knth             -- A      -- | A  .   + B | 2^n
                    + f (a - knth) b (knth + v) -- B      -- +--------+---+

    | b /= nth && a <= 2 * nth            -- A   --   nth  a-nth
        = nth * sq nth                    -- B   -- +-----+---+       
              + f (a - nth) nth (v + nth) -- C   -- |  A  | B | nth   
              + f nth (b - nth) (v + nth) -- D   -- |     |   |       
              + f (a - nth) (b - nth) v          -- +-----+---+       
                                                 -- |  C  | D | b-nth 
                                                 -- +-----+---+       

--                                         n^(n+k+1)
--    n^2   n^2                            | a - n^(n+k+1)
--  +-----+-----+------------------ ~ -----+---+
--  |  A  |  B  |                          |   |
--  |     |     |    E                     | F |
--  +-----+-----+                          |   |
--  |  C  |  D  |                          |   |
--  +-----+-----+------------------ ~ -----+---+

    | otherwise =
        let k = floorLog2 (a // (2 * nth))
        in f (2 * nth) b v -- ABCD 
           + foldl (+) 0 [f (2^(n+k+1-j)) b (v + 2^(n+k+1-j)) | j <- [1..k]] -- E
           + f (a - 2 ^ (n+k+1)) b (v + 2 ^ (n+k+1)) -- F

    where                                  
      n = floorLog2 b
      nth = 2^n

      sq :: Integer -> Integer
      sq x | v >= l  = (v-l)*x + ssum (x-1)
           | v + x > l - 1 = ssum (x+v-l-1)
           | otherwise = 0

      ssum :: Integer -> Integer
      ssum x = x * (x + 1) // 2 

      floorLog2 :: Integer -> Integer
      floorLog2 x = f x where
        f x | x <= 0 = error "Uh oh, that wasn't supposed to happen"
            | x == 1 = 0
            | otherwise = 1 + f (B.rotateR x 1) 
