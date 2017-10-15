module Aula101 where

-- instance Monad Maybe where
--     return x = Just x
--     Nothing >>= f = Nothing
--     (Just x) >>= f = f x

g :: Int -> Maybe Int
g z = Just z >>= \x -> Just (x+1) >>= \y -> Just(x*y)

g1 :: Int -> Maybe Int
g1 z = do
    x <- Just z   
    y <- Just (x+1) 
    return(x*y)