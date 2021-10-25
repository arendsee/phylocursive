{-# LANGUAGE PolyKinds, TypeApplications, DeriveFunctor, OverloadedStrings #-}

module Nat
  ( Nat(..)
  , toNat
  , fromNat
  , factorial
  ) where

import Schema

data Nat a = Zero | Succ a
  deriving(Show, Ord, Eq, Functor)

toNat :: Integer -> Term Nat
toNat 0 = In Zero
toNat i = In (Succ (toNat (i-1))) 

fromNat :: Term Nat -> Integer
fromNat = cata go where
  go Zero = 0
  go (Succ x) = 1 + x

factorial :: Term Nat -> Integer
factorial = para' go where
  go _ Zero = 1
  go t (Succ x) = fromNat t * x
