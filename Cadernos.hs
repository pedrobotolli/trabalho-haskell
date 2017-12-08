module Cadernos where

main :: IO()
main = do
    txtPrimLinha <- fmap (words) getLine
    let valPrimLinha = map (read :: String -> Int) txtPrimLinha 
    print valPrimLinha
    txtSegLinha <- fmap (words) getLine
    let valSegLinha = map (read :: String -> Int) txtSegLinha
    print valSegLinha
    let cadernos =  (foldl (+) 0 valSegLinha)/(valPrimLinha !! 1)
    if ((length valSegLinha) == (read (valPrimLinha !! 0)::Int)) then (print cadernos) else (putStrLn "fudido")
    