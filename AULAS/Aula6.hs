module Aula6 where

data Status = Magro | Gordo | Saudavel deriving Show
data Bolsa a b = Nada | UmItem a | DoisItens a b deriving Show
--TIPO FANTASMA / Ghost Typing
data Metros a = Metros Double deriving Show

data Um
data Dois 

--Recursao
fat:: Int -> Int
fat n
    |n<=1=1
    |otherwise = n*fat(n-1)

modulo:: Double->Double
modulo x
    |x>=0 = x
    |otherwise = -x

--Recursao em Strings
elimVogal :: String -> String 
elimVogal [] = []
elimVogal (x:xs)
    |elem x "AEIOUaeiou" = elimVogal (xs)
    |otherwise = x : elimVogal xs
    
imc :: Double -> Double -> Status
imc peso altura
    |z <= 18 = Magro
    |z <= 25 = Saudavel
    |otherwise  = Gordo
    where
        z = peso / (altura*altura)

--areaQuadrado :: Metros -> Metros
--areaQuadrado (Metros 1 m) = (Metros 2 (m*m))
--areaQuadrado _ = MetragemInvalida
areaQuadrado :: Metros Um -> Metros Dois
areaQuadrado (Metros m) = Metros(m*m)
