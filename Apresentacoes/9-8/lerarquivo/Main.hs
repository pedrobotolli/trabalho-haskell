module Main where

import Text.Printf
import Data.Typeable


main ::IO ()
main = do
    lista <- fmap (map words . lines) $ readFile "d"
    print lista
    print (show(typeOf lista))
    let comparacao = [max (parseInt (x!!0)) (parseInt (x!!1))| x<-lista]
    print comparacao
    
parseInt :: String -> Int
parseInt x = read x :: Int



--    let novaLista = [max (read (x !! 0)) (read (x !! 1)) | x <- lista ]
--    novaLista <- map (read) lista
--    print novaLista

--    salarios <- return $ map (\xs -> [y | x!!2 <- xs]) lista :: [String]
--    print salarios
    
--    salarios <- return $ map (\(_:vl:_) -> read vl ) lista 
--    print "%.2f\n" sum salarios
--    print $ maximum salarios
