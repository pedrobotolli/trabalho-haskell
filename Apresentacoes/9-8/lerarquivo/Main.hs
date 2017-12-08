module MaiorArquivo where

import Text.Printf
import Data.Typeable


main ::IO ()
main = do
    lista <- fmap (map words . lines) $ readFile "d"
    print lista
    print (show(typeOf lista))
    comparacao = [max (parseInt (x!!0)) (parseInt (x!!1))| x<-lista]
    print comparacao
    
parseInt :: String -> Int
parseInt x = read x :: Int


