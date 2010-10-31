{-# LANGUAGE NoGenericDeriving #-}

module Hand.Eq.Logic where

import Auxiliary.Logic (Logic(..), biggerLogic, tweakLogic)
import Auxiliary.Auxiliary (test)
import UHC.Base

deriving instance Show Logic

main :: IO ()
main = test . putStr . show . last . show $ id biggerLogic