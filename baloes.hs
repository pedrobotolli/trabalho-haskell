module Main where

main :: IO ()
main = do
    txtPrimLinha <- fmap (words) getLine
    let valPrimLinha = map (read :: String -> Float) txtPrimLinha 
    let numPerguntas = valPrimLinha !! 0
    let dinheiro = valPrimLinha !! 1
    txtSegLinha <- fmap (words) getLine
    let valSegLinha = map (read :: String -> Float) txtSegLinha
    let soma = foldl (+) 0 valSegLinha
    let divisao = floor (dinheiro/soma)
    print divisao