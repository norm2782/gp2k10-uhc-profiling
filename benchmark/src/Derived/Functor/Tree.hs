module Derived.Functor.Tree where

import Auxiliary.Tree (smallerTree, sumTree, Tree(..))
import Auxiliary.Auxiliary (test, apply)

deriving instance Functor Tree

-- Map on a Tree
-- mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree _  Leaf       = Leaf
-- mapTree f (Bin x l r) = Bin (f x) (mapTree f l) (mapTree f r)

main :: IO ()
main = let t = apply 10 (fmap (+1)) smallerTree
           in test . putStr . show . sumTree $ t
