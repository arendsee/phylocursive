{-# LANGUAGE DeriveFunctor, DeriveFoldable #-}

module FingerTree
  ( FingerTree(..)
  , (|>)
  , (<|)
  , toTree
  , headMay
  , tailMay
  , initMay
  , lastMay
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
(<|) x1 (Deep (D1 x2) down rhs) = Deep (D2 x1 x2) down rhs
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

toTree :: Foldable f => f a -> FingerTree a
toTree = foldr (<|) Empty

toDigit :: Node a -> Digit a
toDigit (Node2 x1 x2) = D2 x1 x2
toDigit (Node3 x1 x2 x3) = D3 x1 x2 x3

data ViewL s a = NilL | ConsL a (s a)

viewL :: FingerTree a -> ViewL FingerTree a
viewL Empty = NilL
viewL (Single x) = ConsL x Empty
viewL (Deep (D1 x1) down rhs) = ConsL x1 (deepL Nothing down rhs)
viewL (Deep (D2 x1 x2) down rhs) = ConsL x1 (deepL (Just (D1 x2)) down rhs)
viewL (Deep (D3 x1 x2 x3) down rhs) = ConsL x1 (deepL (Just (D2 x2 x3)) down rhs)
viewL (Deep (D4 x1 x2 x3 x4) down rhs) = ConsL x1 (deepL (Just (D3 x2 x3 x4)) down rhs)

deepL :: Maybe (Digit a) -> FingerTree (Node a) -> Digit a -> FingerTree a
deepL Nothing deep rhs = case viewL deep of
  NilL -> toTree rhs
  ConsL lhs deep' -> Deep (toDigit lhs) deep' rhs
deepL (Just lhs) deep rhs = Deep lhs deep rhs

headMay :: FingerTree a -> Maybe a
headMay t = case viewL t of
  NilL -> Nothing
  (ConsL x _) -> Just x

tailMay :: FingerTree a -> Maybe (FingerTree a)
tailMay t = case viewL t of
  NilL -> Nothing
  (ConsL _ t) -> Just t

data ViewR s a = ConsR (s a) a | NilR

viewR :: FingerTree a -> ViewR FingerTree a
viewR Empty = NilR
viewR (Single x) = ConsR Empty x
viewR (Deep lhs down (D4 x1 x2 x3 x4)) = ConsR (deepR lhs down (Just (D3 x1 x2 x3))) x4
viewR (Deep lhs down (D3 x1 x2 x3   )) = ConsR (deepR lhs down (Just (D2 x1 x2   ))) x3
viewR (Deep lhs down (D2 x1 x2      )) = ConsR (deepR lhs down (Just (D1 x1      ))) x2
viewR (Deep lhs down (D1 x1         )) = ConsR (deepR lhs down Nothing             ) x1

deepR :: Digit a -> FingerTree (Node a) -> Maybe (Digit a) -> FingerTree a
deepR lhs deep Nothing = case viewR deep of 
  NilR -> toTree lhs
  (ConsR deep' rhs) -> Deep lhs deep' (toDigit rhs)
deepR lhs deep (Just rhs) = Deep lhs deep rhs

lastMay :: FingerTree a -> Maybe a
lastMay tree = case viewR tree of
  NilR -> Nothing
  (ConsR _ x) -> Just x

initMay :: FingerTree a -> Maybe (FingerTree a)
initMay tree = case viewR tree of
  NilR -> Nothing
  (ConsR t _) -> Just t
