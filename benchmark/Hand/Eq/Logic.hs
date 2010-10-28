{-# LANGUAGE StandaloneDeriving #-}

module Hand.Eq.Main where

import Auxiliary.Logic (Logic(..), biggerLogic, tweakLogic)
import Auxiliary.Auxiliary (test)

main :: IO ()
main = test . putStr . show . last . show $ 
             [ biggerLogic == biggerLogic, 
               biggerLogic == tweakLogic biggerLogic,
               tweakLogic biggerLogic == biggerLogic]

