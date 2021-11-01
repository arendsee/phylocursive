{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module FingerTree
  ( FingerTree(..)
  , (|>)
  , (<|)
  ) where


-- 2-3-tree at specific depth
data Node a = Node2 a a | Node3 a a a
  deriving(Show, Ord, Eq, Functor, Foldable)


data Digit a
  = D1 a
  | D2 a a
  | D3 a a a
  | D4 a a a a
  deriving(Show, Ord, Eq, Functor, Foldable)


data FingerTree a
  = Empty
  | Single a
  | Deep (Digit a) (FingerTree (Node a)) (Digit a)
  deriving(Show, Ord, Eq, Functor, Foldable)


infixr 5 <|

(<|) :: a -> FingerTree a -> FingerTree a
(<|) x Empty = Single x
(<|) x (Single y) = Deep (D1 x) Empty (D1 y)
(<|) x1 (Deep (D1 x2) down rhs) = Deep (D2 x1 x1) down rhs
(<|) x1 (Deep (D2 x2 x3) down rhs) = Deep (D3 x1 x2 x3) down rhs
(<|) x1 (Deep (D3 x2 x3 x4) down rhs) = Deep (D4 x1 x2 x3 x4) down rhs
(<|) x1 (Deep (D4 x2 x3 x4 x5) node rhs) = Deep (D2 x1 x2) (Node3 x3 x4 x5 <| node) rhs


infixr 5 |>

(|>) :: FingerTree a -> a -> FingerTree a
(|>) Empty y = Single y
(|>) (Single x) y = Deep (D1 x) Empty (D1 y)
(|>) (Deep lhs down (D1 y1)) y2 = Deep lhs down (D2 y1 y2)
(|>) (Deep lhs down (D2 y1 y2)) y3 = Deep lhs down (D3 y1 y2 y3)
(|>) (Deep lhs down (D3 y1 y2 y3)) y4 = Deep lhs down (D4 y1 y2 y3 y4)
(|>) (Deep lhs down (D4 y1 y2 y3 y4)) y5 = Deep lhs (down |> Node3 y1 y2 y3) (D2 y4 y5)
