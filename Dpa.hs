module Dpa where

main :: IO ()
main = do 
    linhas <- getLine
    let vzs = read linhas
    if (1<= vzs) then (vezes vzs []) else (print "erro...")

vezes :: Int -> [String] -> IO ()
vezes vzs [] = do
    linha <- getLine
    let n = read linha
    let lista= [x | x<-[1..n-1], n `mod` x == 0]
    let soma=(foldl (+) 0 lista)
    if (soma<n) then (vezes (vzs -1) ("deficiente")) else if (soma == n) then (vezes (vzs -1) ("perfeito")) else (vezes (vzs -1) ("abundante")) 
vezes 1 xs = do
    linha <- getLine
    let n = read linha
    let lista= [x | x<-[1..n-1], n `mod` x == 0]
    let soma=(foldl (+) 0 lista)
    if (soma<n) then (exibe (xs++"deficiente")) else if (soma == n) then (exibe (xs++"perfeito")) else (exibe (xs++"abundante")) 
vezes vzs xs = do
    linha <- getLine
    let n = read linha
    let lista= [x | x<-[1..n-1], n `mod` x == 0]
    let soma=(foldl (+) 0 lista)
    if (soma<n) then (vezes (vzs -1) (xs++"deficiente")) else if (soma == n) then (vezes (vzs -1) (xs++"perfeito")) else (vezes (vzs -1) (xs++"abundante")) 
    

exibe :: [String] -> IO ()
exibe x = do
    map (putStrLn) (x)