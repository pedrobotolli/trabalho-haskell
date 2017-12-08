module MaiorDaLinha where

import Text.Printf

main ::IO ()
main = do
    lista <- fmap (map words . lines) $ readFile "d"
    print lista
    comparacao <- return [max (parseInt (x!!0)) (parseInt (x!!1))| x<-lista]
    print comparacao
    maiores <- return [show x |x<-comparacao]
    escreverArquivo maiores True
    
parseInt :: String -> Int
parseInt x = read x :: Int

escreverArquivo :: [String] -> Bool -> IO()
escreverArquivo [] _ = do
    print "os resultado esta no arquivo maiores"
escreverArquivo (x:xs) True = do
    writeFile "maiores" (read (show x)++"\n")
    escreverArquivo xs False
escreverArquivo (x:xs) False = do
    appendFile "maiores" (read (show x)++"\n")
    escreverArquivo xs False
    




--    import Data.Typeable
--    let novaLista = [max (read (x !! 0)) (read (x !! 1)) | x <- lista ]
--    novaLista <- map (read) lista
--    print novaLista

--    salarios <- return $ map (\xs -> [y | x!!2 <- xs]) lista :: [String]
--    print salarios
    
--    salarios <- return $ map (\(_:vl:_) -> read vl ) lista 
--    print "%.2f\n" sum salarios
--    print $ maximum salarios
