{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Schema
 ( Term(..)
 , Algebra
 , Coalgebra
 , cata
 , ana
 , bottomUp
 ) where

import Control.Arrow ((>>>), (&&&), (<<<))
-- (&&&) :: (a -> b) -> (a -> c) -> (b, c)


(&) :: a -> (a -> b) -> b
(&) = flip ($)

newtype Term f = In { out :: f (Term f) }

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
type RAlgebra f a = f (Term f, a) -> a
type RAlgebra' f a = Term f -> f a -> a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn

para :: Functor f => RAlgebra f a -> Term f -> a
para fn = out >>> fmap (id &&& para fn) >>> fn

para' :: Functor f => RAlgebra' f a -> Term f -> a
para' fn t = out t & fmap (para' fn) & fn t

cata' :: Functor f => Algebra f a -> Term f -> a
cata' f = para' (const f)

bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = cata (In >>> fn)
