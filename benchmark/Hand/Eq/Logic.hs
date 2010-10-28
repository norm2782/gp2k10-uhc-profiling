{-# LANGUAGE NoGenericDeriving #-}

module Hand.Eq.Logic where

import Auxiliary.Logic (Logic(..), biggerLogic, tweakLogic)
import Auxiliary.Auxiliary (test)

instance Eq Logic where
  Var s       == Var   t     = s == t
  T           == T           = True
  F           == F           = True
  Not   l1    == Not   l2    = l1 == l2
  Impl  l1 l2 == Impl  l3 l4 = (l1 == l3) && (l2 == l4)
  Equiv l1 l2 == Equiv l3 l4 = (l1 == l3) && (l2 == l4)
  Conj  l1 l2 == Conj  l3 l4 = (l1 == l3) && (l2 == l4)
  Disj  l1 l2 == Disj  l3 l4 = (l1 == l3) && (l2 == l4)
  _           == _           = False


main :: IO ()
main = test . putStr . show . last . show $ 
             [ biggerLogic == biggerLogic, 
               biggerLogic == tweakLogic biggerLogic,
               tweakLogic biggerLogic == biggerLogic]

