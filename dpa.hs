module Main where

main :: IO ()
main = do 
    vezes <- readLn
    preencheLista (0) vezes []

preencheLista :: Int -> Int -> [String] -> IO()
preencheLista a b c  
    |a == (b) = do 
        finalmente 0 b c 
    |a == 0 = do
        linha <- readLn
        let lista= [x | x<-[1..linha-1], mod linha x == 0]
        let soma=(foldl (+) 0 lista)
        let result = if (linha == soma) then ( "perfeito") else if (soma < linha) then ( "deficiente") else ("abundante")
        preencheLista (a+1) b (result:[])
    |a < (b) = do
        linha <- readLn
        let lista= [x | x<-[1..linha-1], mod linha x == 0]
        let soma=(foldl (+) 0 lista)
        let result = if (linha == soma) then ("perfeito") else if (soma < linha) then ( "deficiente") else ("abundante")
        preencheLista (a+1) b (c ++ (read result)) 
    |otherwise = do
        putStrLn "erro..."
    
finalmente :: Int -> Int -> [String] -> IO()
finalmente a b c 
    | a == (b) = do
        print $ c !! a
    | a < (b) = do
        print $ c !! a
        finalmente (a+1) b c 
    | otherwise = do
        putStrLn "erro..."