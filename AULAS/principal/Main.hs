module Main where

-- IO () == void
-- >> eh usado quando ha IO ()
safeHead :: [] a -> Maybe a
safeHead [] = Nothing
safeHead xs = Just(head xs)


main :: IO ()
main = do
    putStrLn "Digite um nome:"
    nome <- fmap safeHead getLine
    case nome of
        Nothing -> putStrLn "Erro..."
        (Just x) -> putStrLn [x]
        
-- case eh o pattern matching no IO
-- funcao que eh pura vem dentro do moned por fmap

main2 :: IO ()
main2 = do 
     putStrLn "Digite um numero: "
     x <- readLn
     putStrLn "Digite outro numero: "
     y <- readLn
     putStrLn  $ "A soma eh: " ++ show(x+y)


main1 :: IO ()
main1 = do
     putStrLn "Digite um nome: " 
     nome <- getLine 
     putStrLn ("OLa " ++ nome) 
