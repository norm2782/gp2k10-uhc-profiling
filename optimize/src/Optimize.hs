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

-- Function definition
data Def = Def Name [Expr] Expr deriving Show



-- Definition 4.5
raise :: Expr -> [Expr] -> State [Def] Expr

raise (Var x) ee                  = return $ App x ee
raise (Cons n dd) ee              = return $ Cons n (dd ++ ee)
raise (Func n dd) ee | delta <= 0 = return $ Func n (dd ++ ee)
                     | otherwise  = State $ \state -> (Func (increase n delta) (dd ++ ee), (Def (name "raise" 0) [] (Var "r")):state)
                                  where
                                      delta = length dd + length ee - arity n                                  
raise (App x dd) ee               = return $ App x (dd ++ ee)


raise (Case x tuples) ee          = mmap update tuples >>= wrap
    where
        wrap tt = return $ Case x tt
        
        update :: (Expr, Expr) -> State [Def] (Expr, Expr)
        update (pattern, expr) = raise expr ee >>= (\updated -> return $ (pattern, updated))


-- Substitute      new  old
data Subst = Subst Expr String



apply :: Subst -> String -> Expr 
apply (Subst new old) for | old == for = new
                          | otherwise  = Var for

                                    
-- Definition 4.7
substitute :: Subst -> Expr -> State [Def] Expr 
substitute s (Var x)      = return $ apply s x
substitute s (App x ee)   = mmap (substitute s) ee >>= raise (apply s x)
substitute s (Func st ee) = mmap (substitute s) ee >>= wrap
    where
        wrap ff = return $ Func st ff
substitute s (Cons st ee) = mmap (substitute s) ee >>= wrap
    where
        wrap ff = return $ Cons st ff




-- Definition 4.8
match :: Int -> Expr -> Def -> State [Def] Expr
match i (Var y)         (Def name arguments (Case x tuples)) = return $ Case y tuples
match i f@(Func _ _)    (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [f] ++ (drop (i + 1) arguments))
match i a@(App _ _)     (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [a] ++ (drop (i + 1) arguments))
match i (Case y tuples) definition = mmap update tuples >>= wrap
    where
        wrap tuples = return $ Case y tuples
        
        update :: (Expr, Expr) -> State [Def] (Expr, Expr)
        update (pattern, expr) = (match i expr definition) >>= (\updated -> return $ (pattern, updated))

match i (Cons n ee)  (Def name arguments (Case x tuples)) = foldl (>>=) (return a) substitutions
    where
        (Cons _ yy, a) = head $ filter (\((Cons m _), _) -> m == n) tuples
        
        substitutions :: [Expr -> State [Def] Expr]
        substitutions = map substitute $ map (\(e, (Var y)) -> Subst e y) $ zip ee yy
            


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
fuse _ _ _ = error "fuse: not yet implemented"


app :: Def
app = Def (name "app" 2) [Var "l", Var "t"] 
    (Case "l" [
        (Cons (name "Nil" 0) [], Var "t"),
        (Cons (name "Cons" 2) [Var "x", Var "xs"], Cons (name "Cons" 2) [Var "x", Func (name "app" 2) [Var "xs", Var "t"]])
    ])
    
foo :: Def
foo = Def (name "foo" 3) [Var "x", Var "y", Var "z"] 
    (Func (name "app" 2) [Func (name "app" 2) [Var "x", Var "y"], Var "z"]) 

main = (putStrLn . show) $ runState (fuse app app 1) []



-- Utility function
mmap :: (a -> State s b) -> [a] -> State s [b]
mmap f []     = return []
mmap f (x:xs) = do h <- f x 
                   t <- mmap f xs
                   return (h:t)