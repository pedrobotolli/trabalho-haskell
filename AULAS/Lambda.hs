module Lambda where
somar :: Int -> Int -> Int -> Int 
somar x y z = x+y+z

-- no ghc (\ x y z -> x+y+z) 1 2 3