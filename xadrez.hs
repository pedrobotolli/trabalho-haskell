module Main where

main :: IO ()
main = do
    let padrao = [1,1,2,2,2,8]
    txtPrimLinha <- fmap (words) getLine
    let valPrimLinha = map (read :: String -> Int) txtPrimLinha 
    let subtracao = [((padrao!!x)-(valPrimLinha!!x))|x<-[0..5]]
    putStrLn $(show (subtracao !! 0)) ++ " " ++ (show (subtracao !! 1)) ++ " " ++ (show (subtracao !! 2)) ++ " " ++ (show (subtracao !! 3)) ++ " " ++ (show (subtracao !! 4)) ++ " " ++ (show (subtracao !! 5))