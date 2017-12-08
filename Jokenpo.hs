module Jokenpo where

import System.Random(randomRIO)

data Jokenpo = Pedra | Papel | Tesoura deriving (Show, Enum, Read, Eq)
-- data type jokenpo
-- derivando os typeclasses show enum read e eq

jogar :: Jokenpo -> Jokenpo -> String
jogar Pedra Papel = "Perdeu"
jogar Papel Pedra = "Ganhou"
jogar Tesoura Papel = "Ganhou"
jogar Papel Tesoura = "Perdeu"
jogar Pedra Tesoura = "Ganhou"
jogar Tesoura Pedra = "Perdeu"
jogar _ _ = "Empatou"


main :: IO ()
main = do
    let jogadas = [Pedra, Papel,Tesoura]
    jogadaNum <- randomRIO (0 , 2)
    jogadaComp <- return $ jogadas !! jogadaNum
    putStrLn "Escreva a sua Jogada: "
    jogada <- fmap (read:: String -> Jokenpo) getLine
    putStrLn $ "A jogada do computador foi " ++  show (jogadaComp)
    putStrLn $ (jogar (jogada) (jogadaComp))
    
    
    
-- jogada <- fmap (read :: String -> Jokenpo) getLine
-- fmap => (f a -> f b) -> f a -> f b
-- fmap => (f String -> f Jokenpo) IO String
-- Read a => String -> a