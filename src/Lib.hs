module Lib
    ( someFunc
    ) where

data Tree a b c = Node a [(b, Tree a b c)] | Leaf c
  deriving(Show, Ord, Eq)

data TreeF a b c r = NodeF a [(b, r)] | LeafF c
  deriving(Show, Ord, Eq)

someFunc :: IO ()
someFunc = do
  let tree = Node "n2" [(12, Leaf "A"), (13, Leaf "B")]
  putStrLn $ show tree
