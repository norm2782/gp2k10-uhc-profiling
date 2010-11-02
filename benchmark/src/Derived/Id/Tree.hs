{-# LANGUAGE GenericDeriving #-}

module Derived.Eq.Tree where

-- import Auxiliary.Tree (Tree(..), bigTree, tweakRightmost)
import Derived.Id.Identity
import System.Random
import Auxiliary.Auxiliary (test)

-- Tree datatype
data Tree a = Leaf | Bin a (Tree a) (Tree a) deriving (Show, Id)

main :: IO ()
main = test . putStr . show $ gid bigTree


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

