module Programa where

data Jokenpo   = Pedra | Papel | Tesoura | Empate deriving (Show)
jokenpo :: Jokenpo -> Jokenpo -> Jokenpo
jokenpo Pedra Papel = Papel
jokenpo Papel Pedra = Papel
jokenpo Tesoura Pedra = Pedra
jokenpo Pedra Tesoura = Pedra
jokenpo Tesoura Papel = Tesoura
jokenpo Papel Tesoura = Tesoura
jokenpo  _ _ = Empate

data Binario = Zero | Um deriving (Show)
eBin :: Binario -> Binario -> Binario
eBin Um Um = Um
eBin _ _ = Zero

proximo :: Int -> Int
proximo x = x + 1

soma :: Int -> Int -> Int
soma x y = x + y

divide :: Double -> Double -> Double
divide x y = x / y

vezes :: Int -> Int -> Int
vezes x y = x * y

eIgual :: Eq a => a -> a -> Bool
eIgual x y = (x == y)