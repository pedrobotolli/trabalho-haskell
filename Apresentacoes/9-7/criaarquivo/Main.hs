module Main where
    
main :: IO ()
main = do
    numVezes <- fmap (read :: String -> Int) getLine
    if numVezes>0  then repetir numVezes else putStrLn "erro..."
       
    
repetir :: Int -> IO ()
repetir 1 = do
    l <- getLine
    appendFile "arquivo.txt" (l ++ "\n")
repetir x = do
    l <- getLine
    appendFile "arquivo.txt" (l ++ "\n")
    repetir $ x - 1
    
    
    

    