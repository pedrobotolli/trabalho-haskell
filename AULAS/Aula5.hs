module Aula5 where

-- HIGH-ORDER FUNCTION: E UMA FUNCAO QUE:
-- A) RECEBE VIA PARAMETRO UMA FUNCAO E/OU
-- B) RETORNA UMA FUNCAO
--foo :: Int -> (Int -> Int) -> Int
--foo x f = x + f 1

dobro :: Int -> Int
dobro x = x*2

g :: (Int -> Int) -> Int
g f = f 1 

h :: Int -> (Int -> Int)
h x = \y -> x*y
-- (h numerox) numeroy

-- CURRYING: E O FATO DE SE PASSAR MENOS ARGUMENTOS QUE O ESPERADO PARA UMA FUNCAO.
-- EX, SE UMA FUNCAO ESPERA 3 PARAMETROS E VC PASSAR 2 ARGUMENTOS, VC TERA EM MAOS UMA FUNCAO QUE ESPERA 1 PARAMETRO
somar :: Int -> Int -> Int -> Int
somar x y z = x+y+z
-- let f = somar 1 2
-- f :: Int -> Int
-- f so vai esperar 1 parametro


data Correncia = Euro | Dollar | Real | Bitcoin deriving Show

-- Record Syntax:  EH O ATO DE SE NOMEAR OS CAMPOS DE UMA VALUE CONSTRUCTOR.
-- O NOME DOS CAMPOS TB SE COMPORTAM COMO "GETS DA POO".
-- ESSES NOMES TB SAO CHAMADOS DE FUNCOES DE PROJECAO.
data Dinheiro = Dinheiro { valor :: Double,
                           curr  :: Correncia
                         } deriving Show

-- map : joga uma funcao em uma lista de elementos
-- filter: filtra os elementos de uma lista de acordo com um predicado (a -> Bool)

tamanho :: String -> Int
tamanho = length


ehPrimo :: Int -> Bool
ehPrimo n = filter (\x -> mod n x == 0) [1..n-1] == [1]

fat :: Int -> Int
fat n = foldl (*) 1 [1 .. n]

--operador ou funcao infixa
infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) x f = f x

foo :: Int -> Int
foo n = n  |> \x -> 2*x |> \y -> y+x+1