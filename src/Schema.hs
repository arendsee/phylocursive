{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Schema
 ( Term(..)
 , Algebra, cata
 , CoAlgebra, ana
 , RAlgebra, para
 , RCoAlgebra, apo
 , Attr(..), CVAlgebra, histo
 , CoAttr(..), CVCoAlgebra, futu
 ) where

import Control.Arrow ((>>>), (&&&), (|||), (<<<))
-- (&&&) :: (a -> b) -> (a -> c) -> (b, c)
-- (|||) :: a b d -> a c d -> a (Either b c) d

(&) :: a -> (a -> b) -> b
(&) = flip ($)

newtype Term f = In { out :: f (Term f) }



-- catamorphism
type Algebra f a = f a -> a
cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

-- anamorphism (co-catamorphism)
type CoAlgebra f a = a -> f a
ana :: Functor f => CoAlgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn



-- paramorphism
type RAlgebra f a = f (Term f, a) -> a
para :: Functor f => RAlgebra f a -> Term f -> a
para fn = out >>> fmap (id &&& para fn) >>> fn

-- apomorphism (co-anamorphism)
type RCoAlgebra f a = a -> f (Either (Term f) a)
apo :: Functor f => RCoAlgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f



-- histomorphism
data Attr f a = Attr
  { attribute :: a
  , hole :: f (Attr f a)
  }

type CVAlgebra f a = f (Attr f a) -> a
histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute where
  worker = out >>> fmap worker >>> (h &&& id) >>> mkAttr
  mkAttr (a, b) = Attr a b

-- futumorphism (co-histomorphism)
data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

type CVCoAlgebra f a = a -> f (CoAttr f a)
futu :: Functor f => CVCoAlgebra f a -> a -> Term f
futu fn = In <<< fmap worker <<< fn where
  worker (Automatic a) = futu fn a
  worker (Manual x) = In (fmap worker x) 
