{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor #-}

module Schema
 ( Term(..)
 , Algebra
 , Coalgebra
 , cata
 , ana
 , bottomUp
 ) where

import Control.Arrow ((>>>), (&&&), (|||), (<<<))
-- (&&&) :: (a -> b) -> (a -> c) -> (b, c)
-- (|||) :: a b d -> a c d -> a (Either b c) d


(&) :: a -> (a -> b) -> b
(&) = flip ($)

newtype Term f = In { out :: f (Term f) }

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a
type RAlgebra f a = f (Term f, a) -> a
type RAlgebra' f a = Term f -> f a -> a

-- the duel of an Ralgebra must reverse both the function arrow AND the concept
-- of a tuple, the reverse of a tuple's tying of things together (product) is
-- an Either's holding of things apart (a sum).
type RCoalgebra f a = a -> f (Either (Term f) a)

cata :: Functor f => Algebra f a -> Term f -> a
cata fn = out >>> fmap (cata fn) >>> fn

ana :: Functor f => Coalgebra f a -> a -> Term f
ana fn = In <<< fmap (ana fn) <<< fn

para :: Functor f => RAlgebra f a -> Term f -> a
para fn = out >>> fmap (id &&& para fn) >>> fn

apo :: Functor f => RCoalgebra f a -> a -> Term f
apo f = In <<< fmap (id ||| apo f) <<< f


data Attr f a = Attr
  { attribute :: a
  , hole :: f (Attr f a)
  }

type CVAlgebra f a = f (Attr f a) -> a

histo :: Functor f => CVAlgebra f a -> Term f -> a
histo h = worker >>> attribute where
  worker = out >>> fmap worker >>> (h &&& id) >>> mkAttr
  mkAttr (a, b) = Attr a b

data CoAttr f a
  = Automatic a
  | Manual (f (CoAttr f a))

type CVCoAlgebra f a = a -> f (CoAttr f a)

futu :: Functor f => CVCoAlgebra f a -> a -> Term f
futu fn = In <<< fmap worker <<< fn where
  worker (Automatic a) = futu fn a
  worker (Manual x) = In (fmap worker x) 


para' :: Functor f => RAlgebra' f a -> Term f -> a
para' fn t = out t & fmap (para' fn) & fn t

cata' :: Functor f => Algebra f a -> Term f -> a
cata' f = para' (const f)

bottomUp :: Functor f => (Term f -> Term f) -> Term f -> Term f
bottomUp fn = cata (In >>> fn)
