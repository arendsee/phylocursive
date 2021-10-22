{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Lib
    ( someFunc
    ) where

import Control.Arrow ((>>>), (<<<))

data Expr a
  = Literal { intVal :: Int }
  | Ident   { name :: String }
  | Index   { target :: a, idx :: a }
  | Unary   { op :: String, target :: a }
  | Binary  { lhs :: a, op :: String, rhs :: a }
  | Call    { func :: a, args :: [a] }
  | Paren   { target :: a }
  deriving (Show, Eq, Functor)

newtype Term f = In { out :: f (Term f) }

ten, add, call :: Term Expr
ten  = In (Literal { intVal = 10 })
add  = In (Ident { name = "add" })
call = In (Call { func = add, args = [ten, ten]}) --add(10, 10)

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn

bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = cata (In >>> fn)

identNames :: Algebra Expr [String]
identNames (Index target idx) = target ++ idx
identNames (Unary s x) = s : x
identNames (Binary lhs s rhs) = s : lhs ++ rhs 
identNames (Call x xs) = x ++ concat xs
identNames (Paren x) = x
identNames (Literal _) = []
identNames (Ident s) = [s]

countNodes :: Algebra Expr Int
countNodes (Literal _) = 1
countNodes (Ident _) = 1
countNodes (Index i j) = 1 + i + j
countNodes (Unary _ x) = 1 + x
countNodes (Binary lhs _ rhs) = 1 + lhs + rhs 
countNodes (Call x xs) = 1 + foldr (+) x xs
countNodes (Paren x) = 1 + x


-- -- Law One
-- cata In x ~~ x
-- 
-- -- Law Two
-- alg :: f a -> a
-- fus :: f a -> f
-- cata (alg >>> fmap func) ~~ (cata alg) >>> func
--
-- ??? the types here don't seem to match?
--
-- Law Three
-- alg :: f a -> a
-- fun :: f a -> g a
-- cata (f >>> In) >>> cata g  ~~  cata (f >>> g)


someFunc :: IO ()
someFunc = putStrLn . show $ cata identNames call


-- import Control.Arrow ((>>>), (<<<))
--
-- data Tree a b c = Node a [(b, Tree a b c)] | Leaf c
--   deriving(Show, Ord, Eq)
--
-- data TreeF a b c r = NodeF a [(b, r)] | LeafF c
--   deriving(Show, Ord, Eq)
--
-- data Term f = In (f (Term f))
--
-- out :: Term f -> f (Term f)
-- out (In t) = t
--
-- bottomUp :: Functor a => (Term a -> Term a) -> Term a -> Term a
-- bottomUp fn =
--   out                     -- unpack
--   >>> fmap (bottomUp fn)  -- recurse
--   >>> In                  -- repack
--   >>> fn                  -- apply
--
-- foldUp :: Functor a => (a -> Term a -> b) -> b -> Term a -> b
-- foldUp fn b0 =
--   out
--
-- type Tree' a b c = Term (TreeF a b c)
--
-- -- instance (Show a, Show b, Show c) => Show Tree' where
-- --   show = bottomUp showF
-- --
-- --   showF (In (LeafF x)) =
--
-- mapLeaf :: (a -> a') -> Tree' a b c -> Tree' a' b c
-- mapLeaf f (In (LeafF x)) = In (LeafF (f x))
-- mapLeaf _ x = x
--
-- someFunc :: IO ()
-- someFunc = do
--   let tree = Node "n2" [(12, Leaf "A"), (13, Leaf "B")]
--   putStrLn $ show tree
--
--   let treef = In (NodeF "n2" [(12, In (LeafF "A")), (12, In (LeafF "B"))])
--   putStrLn $ show treef
