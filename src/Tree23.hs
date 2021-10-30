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
  deriving(Show, Eq, Ord)

empty :: Tree23 a
empty = Nil23

singleton :: a -> Tree23 a
singleton x = Node2 x empty empty

fromList :: (Show a, Ord a) => [a] -> Tree23 a
fromList = foldl (flip insert) empty 

lookupNearest :: (Show a, Num a, Ord a) => a -> Tree23 a -> Maybe a
lookupNearest _ Nil23 = Nothing
lookupNearest _ (Node2 y Nil23 Nil23) = Just y
lookupNearest x (Node3 y1 y2 Nil23 Nil23 Nil23) = Just $ nearest x y1 y2
lookupNearest x (Node2 y lhs rhs)
  | x < y = nearest x y <$> lookupNearest x lhs
  | True  = nearest x y <$> lookupNearest x rhs
lookupNearest x (Node3 y z lhs mid rhs)
  | x < y = nearest x y <$> lookupNearest x lhs
  | x < z = nearest3 x z y <$> lookupNearest x mid
  | True  = nearest x z <$> lookupNearest x rhs

nearest :: (Num a, Ord a) => a -> a -> a -> a 
nearest x y z
  | abs(x - y) < abs(x - z) = y
  | True = z

nearest3 :: (Num a, Ord a) => a -> a -> a -> a -> a 
nearest3 x y1 y2 y3 = nearest x y1 $ nearest x y2 y3

data Result a
  = Full (Tree23 a)
  | Part a (Result a) (Result a)
  | Full2 a (Result a) (Result a)
  | Full3 a a (Result a) (Result a) (Result a)
  deriving(Eq, Ord, Show)

fromResult :: Result a -> Tree23 a
fromResult (Full x) = x
fromResult (Part x lhs rhs) = Node2 x (fromResult lhs) (fromResult rhs)
fromResult (Full2 n lhs rhs) = Node2 n (fromResult lhs) (fromResult rhs)
fromResult (Full3 n1 n2 lhs mid rhs) = Node3 n1 n2 (fromResult lhs) (fromResult mid) (fromResult rhs)

deleteMin :: Ord a => Tree23 a -> Tree23 a
deleteMin = undefined

deleteMax :: Ord a => Tree23 a -> Tree23 a
deleteMax = undefined

deleteIdx :: Ord a => Tree23 a -> Tree23 a
deleteIdx = undefined

deleteVal :: Ord a => Tree23 a -> Tree23 a
deleteVal = undefined

insert :: (Show a, Ord a) => a -> Tree23 a -> Tree23 a
insert x = fromResult . unfour . insert' x

insert' :: Ord a => a -> Tree23 a -> Result a
-- base cases
insert' x Nil23 = Full $ Node2 x empty empty
insert' x (Node2 y Nil23 Nil23)
  | x < y = Full $ Node3 x y empty empty empty
  | True  = Full $ Node3 y x empty empty empty
insert' x (Node3 y z Nil23 Nil23 Nil23)
  | x < y     = Part y (Full $ Node2 x empty empty) (Full $ Node2 z empty empty)
  | x < z     = Part x (Full $ Node2 y empty empty) (Full $ Node2 z empty empty)
  | otherwise = Part z (Full $ Node2 y empty empty) (Full $ Node2 x empty empty)
-- node2
insert' x (Node2 y lhs rhs)
  | x < y = unfour (Full2 y (insert' x lhs) (Full rhs))
  | True  = unfour (Full2 y (Full lhs) (insert' x rhs))
-- node3
insert' x (Node3 y z lhs mid rhs)
  | x < y = unfour (Full3 y z (insert' x lhs) (Full mid) (Full rhs))
  | x < z = unfour (Full3 y z (Full lhs) (insert' x mid) (Full rhs))
  | True  = unfour (Full3 y z (Full lhs) (Full mid) (insert' x rhs))


unfour :: Ord a => Result a -> Result a

-- insert into a 3-node with a 2-node parent
--         (n1)           (n2 n1)
--       ~/~   \          /  |  \
--      (n2)    rhs ==> t1   t2  rhs
--     /   \
--    t1   t2
unfour (Full2 n1 (Part n2 t1 t2) rhs) =
  Full . fromResult $ Full3 n2 n1 t1 t2 rhs

--     (n1)                (n1 n2)
--    /   ~\~              /  |   \
-- lhs     (n2)   ==>   lhs   t1   t2
--        /    \
--      t1      t2
unfour (Full2 n1 lhs (Part n2 t1 t2)) =
  Full . fromResult $ Full3 n1 n2 lhs t1 t2

-- insert into a 3-node with a 3-node parent
--                            (n2) 
--                          ~/~  ~\~
--       (n2 n3)            n1    n3
--     ~/~  |  \           /  \  /  \
--    (n1)  t3  t4 ==>   t1  t2 t3  t4
--   /    \
-- t1      t2
unfour (Full3 n2 n3 (Part n1 t1 t2) t3 t4) =
  Part n2 (Full2 n1 t1 t2) (Full2 n3 t3 t4)

--                           (n2)
--                         ~/~  ~\~
--    (n1 n3)             (n1)   (n3)
--   /  ~|~  \            /  \   /  \
--  t1  (n2)  t4 ==>    t1  t2  t3  t4
--      /  \
--     t2  t3
unfour (Full3 n1 n3 t1 (Part n2 t2 t3) t4) = 
  Part n2 (Full2 n1 t1 t2) (Full2 n3 t3 t4)

--                         (n2)
--                       ~/~  ~\~
--    (n1 n2)           (n1)   (n3)
--   /  ~|~  \          /  \   /  \
--  t1  t2   (n3) ==>  t1  t2  t3  t4
--           /  \
--          t3  t4
unfour (Full3 n1 n2 t1 t2 (Part n3 t3 t4)) =
  Part n2 (Full2 n1 t1 t2) (Full2 n3 t3 t4)

unfour x@(Full _) = x

unfour x@(Part _ _ _) = x

unfour (Full2 n (Full t1) (Full t2)) = Full $ Node2 n t1 t2

unfour (Full3 n1 n2 (Full t1) (Full t2) (Full t3)) = Full $ Node3 n1 n2 t1 t2 t3
