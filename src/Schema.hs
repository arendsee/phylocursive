{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Schema
 ( Term(..)
 , Algebra
 , Coalgebra
 , cata
 , ana
 , bottomUp
 ) where

import Control.Arrow ((>>>), (<<<))

newtype Term f = In { out :: f (Term f) }

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn

bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = cata (In >>> fn)
