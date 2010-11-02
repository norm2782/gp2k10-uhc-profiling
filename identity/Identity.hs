{-# LANGUAGE GenericDeriving #-}

module Identity where

import UHC.Base

class Id a where
    gid :: a -> a 
    
instance Id Integer where
    gid = id
    

class Id' f where
  gid' :: f a -> f a

instance Id' U1 where
  gid' U1 = U1

instance Id' (K1 i c) where
  gid' (K1 a) = K1 a

-- No instances for P or Rec because gid is only applicable to types of kind *

instance (Id' a) => Id' (M1 i c a) where
  gid' (M1 a) = M1 $ gid' a

instance (Id' a, Id' b) => Id' (a :+: b) where
  gid' (L1 a) = L1 $ gid' a
  gid' (R1 a) = R1 $ gid' a

instance (Id' a, Id' b) => Id' (a :*: b) where
  gid' (a :*: b) = (gid' a) :*: (gid' b)

{-# DERIVABLE Id gid giddefault #-}
deriving instance (Id a) => Id (Maybe a)
deriving instance (Id a) => Id [a]


giddefault :: (Representable0 a rep0, Id' rep0) => rep0 x -> a -> a
giddefault (rep :: r) x = to0 $ gid' (from0 x :: r) 



data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show, Id)

main = (putStrLn . show) $ gid (Node (Leaf 1) (Leaf 2))