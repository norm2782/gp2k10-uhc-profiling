{-# LANGUAGE GenericDeriving #-}

module Derived.Eq.Logic where

import Derived.Id.Identity
import Auxiliary.Logic (Logic(..), biggerLogic, tweakLogic)
import Auxiliary.Auxiliary (test)

deriving instance Id Logic

main :: IO ()
main = test . putStr . show . last . show $ gid biggerLogic
