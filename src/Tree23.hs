module Tree23
 ( Tree23 (..)
 , fromList
 , empty
 , singleton
 , lookupNearest
 , insert
 ) where

import qualified Data.List as DL

data Tree23 a
  = Nil23
  | Node2 a (Tree23 a) (Tree23 a)
  | Node3 a a (Tree23 a) (Tree23 a) (Tree23 a)
  -- transient, should never exist in the final tree
  | Node4 a a a (Tree23 a) (Tree23 a) (Tree23 a) (Tree23 a)
  deriving(Show, Eq, Ord)

empty :: Tree23 a
empty = Nil23

singleton :: a -> Tree23 a
singleton x = Node2 x empty empty

fromList :: (Show a, Ord a) => [a] -> Tree23 a
fromList = foldl (flip insert) empty 

lookupNearest :: (Show a, Num a, Ord a) => a -> Tree23 a -> Maybe a
lookupNearest _ Nil23 = Nothing
lookupNearest x (Node2 y lhs rhs)
  | x < y = nearest x y <$> lookupNearest x lhs
  | True  = nearest x y <$> lookupNearest x rhs
lookupNearest x (Node3 y z lhs middle rhs)
  | x < y = nearest x y <$> lookupNearest x lhs
  | x < z = nearest3 x z y <$> lookupNearest x lhs
  | True  = nearest x z <$> lookupNearest x lhs
lookupNearest _ tree = error $ "bad case: " ++ show tree

nearest :: (Num a, Ord a) => a -> a -> a -> a 
nearest x y z
  | abs(x - y) < abs(x - z) = y
  | True = z

nearest3 :: (Num a, Ord a) => a -> a -> a -> a -> a 
nearest3 x y1 y2 y3 = nearest x y1 $ nearest x y2 y3

insert :: (Show a, Ord a) => a -> Tree23 a -> Tree23 a
insert x tree = unfour $ insert' x tree

insert' :: (Show a, Ord a) => a -> Tree23 a -> Tree23 a
-- base cases
insert' x Nil23 = Node2 x empty empty
insert' x (Node2 y Nil23 Nil23)
  | x < y = Node3 x y empty empty empty
  | True  = Node3 y x empty empty empty
insert' x (Node3 y z Nil23 Nil23 Nil23)
  | x < y     = Node4 x y z empty empty empty empty
  | x < z     = Node4 y x z empty empty empty empty
  | otherwise = Node4 y z x empty empty empty empty
-- node2
insert' x (Node2 y lhs rhs)
  | x < y = unfour (Node2 y (insert' x lhs) rhs)
  | True  = unfour (Node2 y lhs (insert' x rhs))
-- node3
insert' x (Node3 y z lhs mid rhs)
  | x < y = unfour (Node3 y z (insert' x lhs) mid rhs)
  | x < z = unfour (Node3 y z lhs (insert' x mid) rhs)
  | True  = unfour (Node3 y z lhs mid (insert' x rhs))
insert' _ tree = error $ "bad case: " ++ show tree

unfour :: Ord a => Tree23 a -> Tree23 a
-- insert into a 3-node with a 2-node parent
--          (n4)           ____(n2 n4)__
--         /    \   ==>   /       |     \
--   (n1 n2 n3)  x6     (n1)     (n3)    x5
--   /  |  |  \        /   \     /  \
--  x1  x2 x3 x4      x1   x2  x3   x4
unfour (Node2 n4 (Node4 n1 n2 n3 x1 x2 x3 x4) x5) =
  Node3 n2 n4 (Node2 n1 x1 x2) (Node2 n3 x3 x4) x5

--  (n1)                 ____(n1 n3)__
--  /  \                /       |     \
-- x1  (n2 n3 n4)  ==> x1      (n2)   (n4)
--     /  |  |  \              /  \   /  \
--   x2  x3 x4  x5           x2  x3  x4  x5
unfour (Node2 n1 x1 (Node4 n2 n3 n4 x2 x3 x4 x5)) =
  Node3 n1 n3 x1 (Node2 n2 x2 x3) (Node2 n4 x4 x5)

-- insert into a 3-node with a 3-node parent
--             __(n4 n5)__                __ (n4)___
--            /     |     \              /          \
--    (n1 n2 n3)    x5    x6   ==>   (n2)__          (n5)
--    /  |  |  \                    /      \        /    \
--  x1  x2  x3  x4                (n1)     (n3)    x5    x6
--                               /   \     /  \
--                             x1     x2  x3   x4
unfour (Node3 n4 n5 (Node4 n1 n2 n3 x1 x2 x3 x4) x5 x6) =
  Node2 n4 (Node2 n2 (Node2 n1 x1 x2) (Node2 n3 x3 x4)) (Node2 n5 x5 x6)

--     __(n1 n5)__                 ___(n3)___
--    /     |     \               /          \
--  x1  (n2 n3 n4) x6   ==>   (n1)          (n5)
--      /  |  |  \            /  \         /    \
--    x2  x3  x4  x5         x1 (n2)     (n4)   x6
--                              /  \     /   \
--                             x2  x3   x4   x5
unfour (Node3 n1 n5 x1 (Node4 n2 n3 n4 x2 x3 x4 x5) x6) = 
  Node2 n3 (Node2 n1 x1 (Node2 n2 x2 x3)) (Node2 n5 (Node2 n4 x4 x5) x6)

--    __(n1 n2)__                     ___(n2)___
--   /     |     \                   /          \
--  x1     x2   (n3 n4 n5)   ==>    (n1)       (n4)__
--              /  |  |  \         /    \     /      \
--            x3  x4  x5  x6      x1    x2  (n3)     (n5)
--                                         /   \     /  \
--                                       x3     x4  x5   x6
unfour (Node3 n1 n2 x1 x2 (Node4 n3 n4 n5 x3 x4 x5 x6)) =
  Node2 n2 (Node2 n1 x1 x2) (Node2 n4 (Node2 n3 x3 x4) (Node2 n5 x5 x6))

unfour (Node4 x y z Nil23 Nil23 Nil23 Nil23)
  | x < y = Node2 y (singleton x) (singleton z)
  | x < z = Node2 x (singleton y) (singleton z)
  | True  = Node2 z (singleton y) (singleton x)

unfour x = x
