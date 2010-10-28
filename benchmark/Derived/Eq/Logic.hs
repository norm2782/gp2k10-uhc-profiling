{-# LANGUAGE GenericDeriving #-}

module Derived.Eq.Logic where

import Auxiliary.Logic (Logic(..), biggerLogic, tweakLogic)
import Auxiliary.Auxiliary (test)

deriving instance Eq Logic

main :: IO ()
main = test . putStr . show . last . show $ 
             [ biggerLogic == biggerLogic, 
               biggerLogic == tweakLogic biggerLogic,
               tweakLogic biggerLogic == biggerLogic]

