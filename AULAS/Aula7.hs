module Aula7 where
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
import Data.monoid


-- TYPE PARAMETER: ESTE PARAMETRO, QUE EH UM TIPO, DEFINIRA O COMPORTAMENTO DE CARTEIRA.
-- EX: Carteira Int, Carteira String, Carteira Bool

data Carteira a = Nada 
                | UmItem a 
                | DoisItens a a deriving Show
                
-- :kind Carteira
-- * -> *


-- FUNCAO PARCIAL: PERIGO! NAO HA DEFINICAO DA FUNCAO NO PATTERN MATCHING Nada
mostrarPrimeiro :: Carteira a -> a
mostrarPrimeiro (UmItem x) = x
mostrarPrimeiro (DoisItens x _) = x

-- TYPE CLASS :: IMPOEM RESTRICOES A TIPOS GENERICOS.
-- VOCE SO CONSEGUIRA TRABALHAR COM AS FUNCOES DEFINIDAS NO TYPECLASS E MAIS NADA.
mostrar :: Show a => Carteira a -> String
mostrar Nada = "Vazio..."
mostrar (UmItem x) = "Elemento: "++ show x
mostrar (DoisItens x y) = "Elemento: "++ show x ++ " "++ show y


data Moeda = Euro | Real | Dollar
-- SE QUISERMOS FAZER UMA IMPLEMENTACAO DE SHOW DIFERENTE DA PROPOSTA PELA LINGUAGEM, DEVEMOS:
-- A) TIRAR O DERIVING

instance Show Moeda where
    show Euro = "Que chique..."
    show Dollar = "Que legal..."
    show Real = "Que ..."
    
instance Eq Moeda where
    Dollar == Euro = True
    Euro == Dollar = True
    Real == Real = True
    Euro == Euro = True
    Dollar == Dollar = True
    _ == _ = False
    
-- Eq eh uma resticao de kind *
-- logo, tivemos que usar Eq (Carteira a)
-- pois, Carteira tem kind * -> *
instance Eq a => Eq (Carteira a) where
    Nada == Nada = True
    (UmItem x) == (UmItem y) = x == y
    (DoisItens x1 y1) == (DoisItens x2 y2) = (x1 == x2 && y1 == y2) || ( y1 == x2 && x1 == y2) 
    _ == _ = False
    
    
-- AULA 8

-- Restricao de kind *
class SimNao a where
    simnao :: a -> Bool
    
-- 0 nao funcionou, nao ha como garantir que Num a possua zero.
-- instance SimNao Int where
--    simnao x
--        | x > 0 = True
--        | otherwise = False


instance (Ord a, Monoid a) SimNao where
    simnao x
        | x > mempty = True
        | otherwise = False
        
instance SimNao [a] where
    simnao [] = False
    simnao _ = True
    
instance SimNao Bool where 
    simnao = id
    
-- monoid:
-- def: um tipo m com uma operacao binaria mappend = (<>) :: m -> m -> m
-- e satisfazendo dois axiomas :
-- o do elemento neutro (mempty): um parametro dado que não vai fazer o retorno da funcao ser igual a o outro parametro dado na monoide 
-- associatividade: a, b, c :: m
-- (a<>b) <> c = a <> (b<>c)
-- quando e soma o elemento neutro (mempty) e igual a 0
-- quando e multiplicacao o elemento neutro e igual a 1
-- em uma string o elemento neutro e igual a []
-- Sum 5 <> Sum 7
-- >Sum {getSum = 12}
-- Product 5 <> Product 7
-- >Product{getProduct = 35}
-- "OLA " <> "MUNDO"
-- >"OLA MUNDO"
-- Sum 8 <> mempty
-- >Sum {getSum = 8}

data And = And Bool deriving Show

instance Monoid And where
    mempty = And True
    mappend (And x) (And y) = And (x && y)
-- mappend (And True) (And True) = And True
-- mappend _ _ = And False

instance Monoid a => Monoid (Carteira a) where
    mempty = Nada
    mappend (UmItem x) (UmItem y) = UmItem (x <> y)
    mappend x Nada = x
    
-- teoria das categorias
-- Category cat 
-- funtores


-- ab e sempre uma funcao q troca a por b
-- fmap :: (a->b) -> Carteira a -> Carteira b
instance Functor Carteira where 
    fmap ab Nada = Nada
    fmap ab (UmItem a) = umItem (ab a)
    fmap ab (DoisItens a1 a2) = DoisItens (ab a1) (ab a2)
    
-- O Funtor Maybe
-- data Maybe a = Nothing | Just a
-- instance Functor Maybe where
--    fmap ab Nothing = Nothing
--    fmap ab (Just a) = Just (ab a)


-- Maybe conserta funcoes parciais
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just(head xs)