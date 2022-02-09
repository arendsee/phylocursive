module ISO where

-- This is from a codewars kata ... I'm really filling this repo with random stuff

-- Two types a and b are isomorphic when there exists a function `ab::a->b` and
-- a function `ba::b->a` such that `ab . ba == id`.

import Data.Maybe (fromJust)
import Data.Void

type ISO a b = (a -> b, b -> a)

-- given ISO a b, we can go from a to b
substL :: ISO a b -> (a -> b)
substL = fst

-- and vice versa
substR :: ISO a b -> (b -> a)
substR = snd

-- There can be more than one ISO a b
isoBool :: ISO Bool Bool
isoBool = (id, id)

isoBoolNot :: ISO Bool Bool
isoBoolNot = (not, not)

-- isomorphism is reflexive
refl :: ISO a a
refl = (id, id)

-- isomorphism is symmetric
symm :: ISO a b -> ISO b a
symm (a, b) = (b, a)

-- isomorphism is transitive
trans :: ISO a b -> ISO b c -> ISO a c
trans (ab, ba) (bc, cb) = (bc . ab, ba . cb)

-- We can combine isomorphism:
isoTuple :: ISO a b -> ISO c d -> ISO (a, c) (b, d)
isoTuple (ab, ba) (cd, dc) = 
  ( \(a, c) -> (ab a, cd c)
  , \(b, d) -> (ba b, dc d)
  )

isoList :: ISO a b -> ISO [a] [b]
isoList (ab, ba) = (map ab, map ba)

isoMaybe :: ISO a b -> ISO (Maybe a) (Maybe b)
isoMaybe (ab, ba) = (fmap ab, fmap ba)

isoEither :: ISO a b -> ISO c d -> ISO (Either a c) (Either b d)
isoEither (ab, ba) (cd, dc) = (f, g) where 
  f (Left a) = Left (ab a)
  f (Right c) = Right (cd c)
  g (Left b) = Left (ba b)
  g (Right d) = Right (dc d)

isoFunc :: ISO a b -> ISO c d -> ISO (a -> c) (b -> d)
isoFunc (ab, ba) (cd, dc) =
  ( \ac -> cd . ac . ba
  , \bd -> dc . bd . ab 
  )

-- Going another way is hard (and is generally impossible)
isoUnMaybe :: ISO (Maybe a) (Maybe b) -> ISO a b
isoUnMaybe (ab, ba) = (unwrap ab, unwrap ba)
  where
    unwrap :: (Maybe a -> Maybe b) -> a -> b
    unwrap f x = case f (Just x) of
      (Just y) -> y
      Nothing -> fromJust $ f Nothing

-- And we have isomorphism on isomorphism!
isoSymm :: ISO (ISO a b) (ISO b a)
isoSymm = (\(a,b) -> (b, a) , \(a,b) -> (b, a))
