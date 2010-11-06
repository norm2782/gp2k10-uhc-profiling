module Hand.Functor.Tree where

import Auxiliary.Tree (smallerTree, sumTree, Tree(..))
import Auxiliary.Auxiliary (test, apply)


-- Map on a Tree
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree _  Leaf       = Leaf
mapTree f (Bin x l r) = Bin (f x) (mapTree f l) (mapTree f r)

main :: IO ()
main = let t = apply 10 (mapTree (+1)) smallerTree
           in test . putStr . show . sumTree $ t
