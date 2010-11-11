module State where

-- Tests with State monad

import Control.Monad.State 

foo :: String -> State Int String
foo input = State $ \state -> (input ++ "!", state + 1)  

{-
bar :: [String] -> State Int String 
bar (x:[]) = do foo x 
bar (x:xs) = do 
             a <- foo x
             bar xs
             -}
mmap :: (a -> State s b) -> [a] -> State s [b]
mmap f []     = return []
mmap f (x:xs) = do h <- f x 
                   t <- mmap f xs
                   return (h:t)


main = (putStrLn . show) $ runState (foo "a" >>= foo) 1


