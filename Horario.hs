module Horario where

import Data.List.Split

main :: IO ()
main = do
    putStrLn "Digite o horário"
    horario <- getLine
    let horarioSeparado = splitOn ":" horario
    putStrLn $ horarioExtenso horarioSeparado 
    
horarioExtenso :: [String] -> String
horarioExtenso [] = "erro....."
horarioExtenso x = "It's " ++ horas (read (x!!0)) ++" "++ minutos (read(x!!1)) ++" "++ ampm (read(x!!0)) 

horas :: Int -> String
horas x 
    | x `mod` 12 == 0 = "twelve"
    | x `mod` 12 == 1 = "one"
    | x `mod` 12 == 2 = "two"
    | x `mod` 12 == 3 = "three"
    | x `mod` 12 == 4 = "four"
    | x `mod` 12 == 5 = "five"
    | x `mod` 12 == 6 = "six"
    | x `mod` 12 == 7 = "seven"
    | x `mod` 12 == 8 = "eight"
    | x `mod` 12 == 9 = "nine"
    | x `mod` 12 == 10 = "ten"
    | x `mod` 12 == 11 = "eleven"
    | otherwise = "erro...."
    
minutos :: Int -> String
minutos x
    | x > 19 && x < 60 = minMaiorVinte x
    | x >= 12 = minMaiorDoze x
    | x < 12 = horas x
    | otherwise = "erro..."
    
minMaiorVinte :: Int -> String
minMaiorVinte x
    | x >= 50 = "fifty"++ (if((x-50)>0) then ("-"++ horas(x-50)) else "")
    | x >= 40 = "forty"++ (if((x-40)>0) then ("-"++ horas(x-40)) else "")
    | x >= 30 = "thirty"++ (if((x-30)>0) then ("-"++ horas(x-30)) else "")
    | x >= 20 = "twenty"++ (if((x-20)>0) then ("-"++ horas(x-20)) else "")
    | otherwise = "erro...."
    
minMaiorDoze :: Int -> String
minMaiorDoze x
    | x == 12 = "twelve"
    | x == 13 = "thirteen"
    | x == 14 = "fourteen"
    | x == 15 = "fifteen"
    | x == 16 = "sixteen"
    | x == 17 = "seventeen"
    | x == 18 = "eighteen"
    | x == 19 = "nineteen"
    | otherwise = "erro...."
    
    
    
ampm :: Int -> String
ampm x
    | x >= 12 = "pm"
    | x < 12 = "am"
    | otherwise = "erro...."