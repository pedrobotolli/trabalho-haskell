module Aula15 where
import Control.Applicative

data Produto = Produto {produtoNome :: String
                        ,produtoPreco :: Double
                        ,produtoEstoque :: Int
                        } deriving Show
                        
-- CASOS DE APLICACAO DE FUNCAO (EXECUTAR A FUNCAO COM UM ARGUMENTO)
-- dobro $ 7+2
-- LADO ESQ: FUNCAO SEM FUNTOR
-- LADO DIR: VALOR SEM FUNTOR (ID)
-- dobro <$> [7+2]
-- LADO ESQ: FUNCAO SEM FUNTOR
-- LADO DIR: VALOR COM FUNTOR
-- [dobro, id, dobro] <*> [7+2]
-- LADO ESQ: FUNCAO COM FUNTOR
-- LADO DIR: VALOR COM FUNTOR
-- mult <$> Just 3 <*> Just 2 <*> Just 7

-- as monadas se diferenciam dos funtores aplicativos pois a monada io dá a possibilidade de sequenciar os eventos 
-- e a notação do facilita a compreensão do codigo funcional