{-# LANGUAGE GenericDeriving #-}

module Derived.Eq.Tree where

-- import Auxiliary.Tree (Tree(..), bigTree, tweakRightmost)
import System.Random
import Auxiliary.Auxiliary (test)

-- Tree datatype
data Tree a = Leaf | Bin a (Tree a) (Tree a) deriving (Show, Eq)

main :: IO ()
main = test . putStr . show $ (bigTree == bigTree, bigTree == (tweakRightmost bigTree))

-- Tweak the rightmost element of a Tree
tweakRightmost :: Tree Int -> Tree Int
tweakRightmost Leaf = Leaf
tweakRightmost (Bin x l Leaf) = Bin (x+1) l Leaf
tweakRightmost (Bin x l r) = Bin x l (tweakRightmost r)


genTree :: [Int] -> Tree Int
genTree []    = Leaf
genTree [_]   = Leaf
genTree (h:t@(_:_)) | even h = Bin h (genTree t) (genTree (tail t))
                    | odd h  = Bin h (genTree (tail t)) (genTree t)


-- Big trees (size 1542685, 36924 even labels, 477304 odd)
bigTrees :: [Tree Int]
bigTrees = [bigTreeGen seed | seed <- [123456789..223456789]]

bigTreeGen :: Int -> Tree Int
bigTreeGen seed = genTree . take 30 $ randomRs (0,100) (mkStdGen seed)

bigTree :: Tree Int
bigTree = head bigTrees

bigTreeString :: String
bigTreeString = error "bigTreeString" --gshow bigTree

