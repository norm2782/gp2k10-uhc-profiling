module TestNG where

data Foo = Bar Int

instance Eq Foo where
    (Bar x) == (Bar y) = x == y
