module Aula3 where

import Data.List


data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Enum, Eq, Show)
-- Pattern Matching: Desconstroi um tipo fazendo que seja revelada sua estrutura interna.
-- no nosso caso foi revelado os VALUE CONSTRUCTORS
agenda :: Dia -> String
agenda Segunda = "Dia de cinema"
agenda Quarta  = "Dia de futebol"
agenda Quinta  = "Dia de praca e nossa"
agenda Sexta   = "Dia de maldade"
agenda Sabado  = "Dia de balada"
agenda _       = "Dia de fazer nada"



diaInt :: Dia -> Int
diaInt Segunda = 1
diaInt Terca = 2
diaInt Quarta=3
diaInt Quinta=4
diaInt Sexta=5
diaInt Sabado=6
diaInt Domingo=7

data Day = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday deriving (Enum, Eq, Show)
traduzirPI :: Dia -> Day
traduzirPI Segunda = Monday
traduzirPI Terca = Tuesday
traduzirPI Quarta = Wednesday
traduzirPI Quinta = Thursday
traduzirPI Sexta = Friday
traduzirPI Sabado = Saturday
traduzirPI Domingo = Sunday

data Curso = ADS | SI | GE | GP | Log deriving Show
data Aluno = Aluno String Int Curso deriving Show

aniversario :: Aluno -> Aluno
aniversario (Aluno nome idade curso) =Aluno nome (idade+1) curso

data Naipe = Ouros | Copas | Espadas | Paus deriving (Enum, Show)
data Valor = As | Dois | Tres | Quatro | Cinco | Seis | Sete | Oito | Nove | Dez | Valete | Dama | Rei deriving (Enum, Show)
data Carta = Carta Valor Naipe deriving Show


{-
dobro :: Double -> Double
dobro x = 2*x

somar :: Int -> Int -> Int -> Int
somar x y z =x+y+z

f :: String -> Int
f ls = 1+ lenght ls

maiorQue :: Int -> Int -> Bool
maiorQue x n = x > n
-} 
