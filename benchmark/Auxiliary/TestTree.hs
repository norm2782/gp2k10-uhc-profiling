module TestTree where

data Tree a = Leaf | Bin a (Tree a) (Tree a) deriving (Eq)
