{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Optimize where

import Control.Monad.State 

import Name hiding (substitute)
import qualified Name as N
    
-- Core functional language
data Expr = Var String
          | Cons Name [Expr]
          | Func Name [Expr]
          | App String [Expr]
          | Case String [(Expr, Expr)]
          deriving Show        
  
{-
Attempt to use a GADT

data Var
data Con
data Fun 
data App
data Case
          
data Expr a where
    Var  :: String                           -> Expr Var
    Con  :: Name     -> [Expr a]             -> Expr Con
    Fun  :: Name     -> [Expr a]             -> Expr Fun
    App  :: Expr Var -> [Expr a]             -> Expr App
    Case :: Expr Var -> [(Expr Con, Expr a)] -> Expr Case
    deriving Show
-}

-- Function definition
data Def = Def Name [Expr] Expr deriving Show



-- Definition 4.5
raise :: Expr -> [Expr] -> State [Def] Expr

raise (Var x) ee                  = return $ App x ee
raise (Cons n dd) ee              = return $ Cons n (dd ++ ee)
raise (Func n dd) ee | delta <= 0 = return $ Func n (dd ++ ee)
                     | otherwise  = error "Not yet implemented" -- State $ \state -> (Func (increase n delta) (dd ++ ee), (increase n delta, raise ))
                                  where
                                      delta = length dd + length ee - arity n                                  
raise (App x dd) ee               = return $ App x (dd ++ ee)


raise (Case x tuples) ee          = mmap update tuples >>= wrap
    where
        wrap tt = return $ Case x tt
        
        update :: (Expr, Expr) -> State [Def] (Expr, Expr)
        update (pattern, expr) = raise expr ee >>= (\updated -> return $ (pattern, updated))


data Subst = Subst String Expr

        
apply :: Subst -> String -> Expr 
apply = undefined    

                                    
-- Definition 4.7
substitute :: Subst -> Expr -> State [Def] Expr 
substitute s (Var x)      = return $ apply s x
substitute s (Func st ee) = mmap (substitute s) ee >>= wrap
    where
        wrap ff = return $ Func st ff
substitute s (Cons st ee) = mmap (substitute s) ee >>= wrap
    where
        wrap ff = return $ Cons st ff
substitute s (App x ee)   = mmap (substitute s) ee >>= raise (apply s x)



-- Definition 4.8
match :: Int -> Expr -> Def -> State [Def] Expr
match i (Var y)      (Def name arguments (Case x tuples)) = return $ Case y tuples
match i f@(Func _ _) (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [f] ++ (drop (i + 1) arguments))
match i a@(App _ _)  (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [a] ++ (drop (i + 1) arguments))
match i (Cons n ee)  (Def name arguments (Case x tuples)) = undefined -- substitute subst expr
    where
        (Cons _ yy, expr) = head $ filter (\((Cons m _), _) -> m == n) tuples
        subst = undefined 
            


-- Definition 4.9
fuse :: Def -> Def -> Int -> State [Def] ()
fuse f@(Def fn xx (Case x tuples)) s@(Def sn zz b) i = State $ \state -> ((), [Def name [] body] ++ more ++ state)
    where
        m = arity fn
        n = arity sn
        
        k = length zz
        
        name         = N.substitute fn (decrease sn (m - k)) i
        (body, more) = runState (match i b f) []


-- 1b & 2: TODO
fuse _ _ _ = undefined


main = (putStrLn . show) $ runState (fuse (Def (name "F" 2) [Var "x", Var "y"] (Case "x" [])) (Def (name "S" 1) [Var "z"] (Var "z")) 2) []



-- Utility function
mmap :: (a -> State s b) -> [a] -> State s [b]
mmap f []     = return []
mmap f (x:xs) = do h <- f x 
                   t <- mmap f xs
                   return (h:t)