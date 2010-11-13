{-# LANGUAGE FlexibleInstances #-}

module Util where

import Control.Monad.State 
import Data.List

import Core
import Name


-- Utility function
mmap :: (a -> State s b) -> [a] -> State s [b]
mmap f []     = return []
mmap f (x:xs) = do h <- f x 
                   t <- mmap f xs
                   return (h:t)
                   
        
class Print a where
    pp :: a -> String
    
instance Print Name where
    pp (Name label variables) = label ++ (concat $ intersperse "" $ map pp variables)
    
instance Print Variable where
    pp (Named n) = "{" ++ pp n ++ "}"
    pp (Anonymous) = "[]"
    
instance Print [Def] where
    pp []     = []
    pp (x:xs) = pp x ++ "\n\n-----------------------------------\n\n" ++ pp xs
    
instance Print Def where
    pp (Def name parameters body) = pp name ++ " " ++ (concat $ intersperse " " $ map pp parameters) ++ " = " ++ pp body
    
instance Print Expr where
    pp (Var l) = l
    pp (Cons name args) = pp name ++ " (" ++ (concat $ intersperse ", " $ map pp args) ++ ")"
    pp (Func name args) = pp name ++ " (" ++ (concat $ intersperse ", " $ map pp args) ++ ")"
    pp (App l args) = l ++ " " ++ (concat $ intersperse " " $ map pp args)
    pp (Case y tuples) = "case " ++ y ++ " of\n\t" ++ (concat $ intersperse "\n\t" $ map showTuple tuples) ++ "\nend"
        where
            showTuple (pattern, expr) = pp pattern ++ " -> " ++ pp expr