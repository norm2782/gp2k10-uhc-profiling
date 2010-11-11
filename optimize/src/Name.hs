module Name where

-- Definition 4.2    
data Name = Name String [Variable] deriving (Show, Eq)

data Variable = Anonymous
              | Named Name
              deriving (Show, Eq)

class Arity a where
    arity :: a -> Int
    substitute :: a -> a -> Int -> a

    
instance Arity Name where
    arity (Name _ tt) = sum $ map arity tt
    substitute name v i = case substitute (Named name) (Named v) i of Named out -> out


instance Arity Variable where
    arity Anonymous = 1
    arity (Named n) = arity n    
    
    substitute Anonymous           v 1 = v
    substitute (Named (Name n tt)) v i = Named (Name n $ take j tt ++ [substitute (tt!!j) v (i - sigma j)] ++ drop (j + 1) tt)
        where
            -- Possible values for j
            indexes = take (length tt) [0,1..]
    
            -- Sum all arities up till the j-th variable
            sigma j = sum $ take j $ map arity tt
    
            -- Find j        
            j = head [j | j <- indexes, 0 < i - sigma j, i - sigma j <= arity (tt!!j)] 
        

-- Increase arity, adds k Anonymous variables
increase :: Name -> Int -> Name
increase (Name n tt) k = Name n $ tt ++ take k (repeat Anonymous)

-- Decrease arity, removes k Anonymous variables
decrease :: Name -> Int -> Name
decrease (Name n tt) 0 = Name n tt
decrease (Name n tt) k = decrease (Name n $ h tt) (k - 1)
    where
        h :: [Variable] -> [Variable]
        h uu = case last uu of 
                   Anonymous             -> init uu
                   (Named t@(Name n vv)) -> if arity t > 0 then init uu ++ [Named (Name n $ h vv)] else h (init uu) ++ [last uu]


name :: String -> Int -> Name 
name l = increase (Name l [])
 
                  
-- main = (putStrLn . show) $ substitute (Named (Name "bar" [(Named (Name "daz" [])), Anonymous, Anonymous])) (Named (Name "foo" [])) 2