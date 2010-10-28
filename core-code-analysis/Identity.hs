{-# LANGUAGE NoGenericDeriving #-}

module ADT where

data Foo = Bar Int

class Id a where
    id :: a -> a
    
instance Id Foo where
    id p = p