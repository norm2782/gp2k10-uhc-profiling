{-# LANGUAGE StandaloneDeriving #-}

module Hand.Eq.Main where

import Auxiliary.Tree (Tree(..), bigTree, tweakRightmost)
import Auxiliary.Auxiliary (test)

main :: IO ()
main = test . putStr . show $ 
            (bigTree == bigTree, bigTree == (tweakRightmost bigTree))

