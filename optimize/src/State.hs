module State where

-- Tests with State monad

import Control.Monad.State 

foo :: String -> State Int String
foo input = State $ \state -> (input ++ "!", state + 1)  

bar :: State Int String 
bar = do 
    foo "Hello"
    foo "World"

main = (putStrLn . show) $ runState bar 1