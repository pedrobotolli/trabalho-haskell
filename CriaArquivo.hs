module CriaArquivo where
    
main :: IO ()
main = do
    numVezes <- fmap (read) getLine
    if numVezes>0  then repetir numVezes else putStrLn "erro..."
       
    
repetir :: Int -> IO ()
repetir 1 = do
    l <- getLine
    appendFile "arquivo.txt" (l ++ "\n")
repetir x = do
    l <- getLine
    appendFile "arquivo.txt" (l ++ "\n")
    repetir $ x - 1
    
-- :: String -> Int