module Optimize where
    
import Control.Monad.State 

import Name hiding (substitute)
import qualified Name as N
import Util
import Core


-- Test functions
app :: Def
app = Def (name "app" 2) [Var "l", Var "t"] 
    (Case "l" [
        (Cons (name "Nil" 0) [], Var "t"),
        (Cons (name "Cons" 2) [Var "x", Var "xs"], Cons (name "Cons" 2) [Var "x", Func (name "app" 2) [Var "xs", Var "t"]])
    ])
    

foo :: Def
foo = Def (name "foo" 3) [Var "x", Var "y", Var "z"] 
    (Func (name "app" 2) [Func (name "app" 2) [Var "x", Var "y"], Var "z"]) 

main = (putStrLn . pp) $ snd $ runState (fuse app app 1) [app, foo]


-- Definition 4.5

-- TODO: Uodate state on line 35
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


-- Substitute      new  old
data Subst = Subst Expr String deriving Show

apply :: Subst -> String -> Expr 
apply (Subst new old) for | old == for = new
                          | otherwise  = Var for


-- Definition 4.8
match :: Bool -> Int -> Expr -> Def -> State [Def] Expr
match True  i (Var y)         (Def name arguments (Case x tuples)) = return $ Case y tuples
match False i (Var y)         (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [Var y] ++ (drop (i + 1) arguments))
match _     i f@(Func _ _)    (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [f] ++ (drop (i + 1) arguments))
match _     i a@(App _ _)     (Def name arguments (Case x tuples)) = return $ Func name ((take i arguments) ++ [a] ++ (drop (i + 1) arguments))
match _     i (Case y tuples) definition = mmap update tuples >>= wrap
    where
        wrap tuples = return $ Case y tuples
        
        update :: (Expr, Expr) -> State [Def] (Expr, Expr)
        update (pattern, expr) = (match True i expr definition) >>= (\updated -> return $ (pattern, updated))

match _     i (Cons n ee)  (Def name arguments (Case x tuples)) = foldl (>>=) (return a) substitutions
    where
        (Cons _ yy, a) = head $ filter (\((Cons m _), _) -> m == n) tuples
        
        substitutions :: [Expr -> State [Def] Expr]
        substitutions = map substitute $ map (\(e, (Var y)) -> Subst e y) $ zip ee yy
            

-- Definition 4.9 - 1a
fuse :: Def -> Def -> Int -> State [Def] ()
fuse f@(Def fn xx (Case x tuples)) s@(Def sn zz b) i = State $ \state -> ((), [Def name parameters body] ++ more ++ state)
    where
        -- This is very confusing in the paper: In def 4.4 m = arity (sn) while in def 4.9 m = arity (fn)
        m = arity sn
        n = arity sn
        
        k = length zz
        
        name         = N.substitute fn (decrease sn (m - k)) i
        (body, more) = runState (match True i b f) []

        parameters = take i xx ++ zz ++ drop (i + 1) xx


-- Definition 4.9 - 1b & 2
fuse _ _ _ = error "def 4.9 1b and 2 are not yet implemented"