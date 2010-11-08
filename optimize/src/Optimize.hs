{-# LANGUAGE GADTs #-}
{-# LANGUAGE EmptyDataDecls #-}

module Optimize where

import Control.Monad.State 
import Name
    
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
data Def = Def Name [String] Expr deriving Show


{-
-- Definition 4.5
raise :: Expr -> [Expr] -> State [Def] Expr
raise (Var x) ee                  = return $ App x ee
raise (Cons n dd) ee              = return $ Cons n (dd ++ ee)
raise (Func n dd) ee | delta <= 0 = return $ Func n (dd ++ ee)
                     | otherwise  = error "Not yet implemented" -- State $ \state -> (Func (increase n delta) (dd ++ ee), (increase n delta, raise ))
                                  where
                                      delta = length dd + length ee - arity n                                  
raise (App x dd) ee               = return $ App x (dd ++ ee)


raise (Case x tuples) ee          = return $ Case x (map update tuples)
                                  where
                                      update (pattern, expr) = (pattern, raise expr ee)
                                      

                                    

fusion :: []

data Subst = Subst String Expr

        
apply :: Subst -> Expr -> Expr 
apply = undefined    

                                    
-- Definition 4.7
substitute :: Subst -> Expr -> State [Def] Expr 
substitute s (Var x)      = return $ apply s x
substitute s (Func st ee) = return $ Func st (map (substitute s) ee)
substitute s (Cons st ee) = return $ Cons st (map (substitute s) ee)
substitute s (App x ee)   = return $ raise (apply s x) (map (subsitute s) ee)

        
-- Definition 4.8
match :: Int -> Expr -> Def -> State [Def] Expr
match i (Var y)     (Def name arguments (Case x tuples)) = return $ Case y tuples
match i func        (Def name arguments (Case x tuples)) = return $ Func name (take i arguments) ++ [func] ++ (drop (i + 1) arguments)
match i app         (Def name arguments (Case x tuples)) = return $ Func name (take i arguments) ++ [app]  ++ (drop (i + 1) arguments)
match i (Cons n ee) (Def name arguments (Case x tuples)) = subsitute ? 
    where
        expr = head $ filter (\((Cons name), expr) -> ) tuples
-}

match = undefined

-- Definition 4.9
fuse :: Def -> Def -> Int -> State [Def] ()
-- 1a
fuse f@(Def fn xx (Case x tuples)) s@(Def sn zz b) i = Def name [] (Var "a")
    where
        m = arity fn
        n = arity sn
        
        k = length zz
        
        name = substitute fn (decrease sn (m - k)) i
        body = match i b f


-- 1b & 2: TODO
fuse _ _ _ = undefined


-- main = (putStrLn . show) $ runState $ fuse (Def $ name "F" 2 $ undefined undefined) (Def $ name "S" 1 $ undefined undefined) $ []
main = (putStrLn . show) $ fuse (Def (name "F" 2) ["x", "y"] (Case "x" [])) (Def (name "S" 1) ["z"] (Var "z")) 2