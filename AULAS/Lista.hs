module Lista where
import Data.Monoid

-- imc :: Double -> Double -> Status
-- imc peso altura
--    |z <= 18 = Magro
--    |z <= 25 = Saudavel
--    |otherwise  = Gordo
--    where
--        z = peso / (altura*altura)

-- CURRYING: E O FATO DE SE PASSAR MENOS ARGUMENTOS QUE O ESPERADO PARA UMA FUNCAO.
-- EX, SE UMA FUNCAO ESPERA 3 PARAMETROS E VC PASSAR 2 ARGUMENTOS, VC TERA EM MAOS UMA FUNCAO QUE ESPERA 1 PARAMETRO
somar :: Int -> Int -> Int -> Int
somar x y z = x+y+z
-- let f = somar 1 2
-- f :: Int -> Int
-- f so vai esperar 1 parametro

-- HIGH-ORDER FUNCTION: E UMA FUNCAO QUE:
-- A) RECEBE VIA PARAMETRO UMA FUNCAO E/OU
-- B) RETORNA UMA FUNCAO
--ex:
--foo :: Int -> (Int -> Int) -> Int
--foo x f = x + f 1

--TIPO FANTASMA / Ghost Typing
data Metros a = Metros Double deriving Show

data Um
data Dois 
areaQuadrado :: Metros Um -> Metros Dois
areaQuadrado (Metros m) = Metros(m*m)

-- TYPE PARAMETER: ESTE PARAMETRO, QUE EH UM TIPO, DEFINIRA O COMPORTAMENTO DE CARTEIRA.
-- EX: Carteira Int, Carteira String, Carteira Bool

-- data Carteira a = Nada 
--                | UmItem a 
--                | DoisItens a a deriving Show
                
-- :kind Carteira
-- * -> *

-- FUNCAO PARCIAL: PERIGO! NAO HA DEFINICAO DA FUNCAO NO PATTERN MATCHING Nada
-- mostrarPrimeiro :: Carteira a -> a
-- mostrarPrimeiro (UmItem x) = x
-- mostrarPrimeiro (DoisItens x _) = x

-- TYPE CLASS :: IMPOEM RESTRICOES A TIPOS GENERICOS.
-- VOCE SO CONSEGUIRA TRABALHAR COM AS FUNCOES DEFINIDAS NO TYPECLASS E MAIS NADA.
-- mostrar :: Show a => Carteira a -> String
-- mostrar Nada = "Vazio..."
-- mostrar (UmItem x) = "Elemento: "++ show x
-- mostrar (DoisItens x y) = "Elemento: "++ show x ++ " "++ show y




-- o operador !! retorna n elemento da lista, começando pelo 0
-- ex: [1,2,3,4]!!1
-- > 2

-- 2.2
stringParOuNao :: String -> Bool
stringParOuNao x = even(length x)
-- even verifica se é par e retorna valor booleano
-- odd faz a mesma coisa so que com impar

-- 2.4
listarTamanhoString :: [String] -> [Int]
listarTamanhoString xs  = [length x| x<-xs]

-- 2.3
reverterStrings :: [String] -> [String]
reverterStrings xs = [reverse x | x<-xs]

-- 2.5 last.reverse

-- 2.6
ePalindromo :: String -> Bool
ePalindromo x = (x == reverse x)

-- 2.7 

intTupla :: Int -> (Int,Int,Int,Int)
intTupla x = (x*2,x*3,x*4,x*5)

-- 3.1
data Pergunta = Sim | Nao
pergNum :: Pergunta -> Int
pergNum Sim = 1
pergNum Nao = 0


listPergs :: [Pergunta] -> [Int]
listPergs xs = [pergNum x | x <- xs]


andPergs :: Pergunta -> Pergunta -> Pergunta
andPergs Sim Sim = Sim
andPergs _ _ = Nao

orPergs :: Pergunta -> Pergunta -> Pergunta
orPergs Nao Nao = Nao
orPergs _ _ = Sim

notPergs :: Pergunta -> Pergunta
notPergs Sim = Nao
notPergs Nao = Sim

-- 3.3
data Jokenpo = Pedra | Papel | Tesoura | Empate

jogar :: Jokenpo -> Jokenpo -> Jokenpo
jogar Pedra Papel = Papel
jogar Papel Pedra = Papel
jogar Tesoura Papel = Tesoura
jogar Papel Tesoura = Tesoura
jogar Pedra Tesoura = Pedra
jogar Tesoura Pedra = Pedra
jogar _ _ = Empate

-- 3.4
semVogais:: [Char] -> [Char]
semVogais xs = [x | x<-xs , x /= 'a' , x /= 'e' , x /= 'i' , x /= 'o' , x /= 'u' , x /= 'A' , x /= 'E' , x /= 'I' , x /= 'O' , x /='U'] 

-- 3.8
eliminaNumeros :: [Int] -> [Int]
eliminaNumeros xs = reverse ([ x | x <- xs , odd x , mod x 7 /= 0, x >= 0 ])

-- 3.9
inverterStrings :: String -> String -> String -> (String,String,String)
inverterStrings x y z = (reverse x , reverse y, reverse z)

-- 3.10 nao faco ideia de como fazer

-- 3.11
data Binario = Zero | Um
data Funcao = Soma2 | Maior | Menor | Mult2
aplicar :: Funcao -> Binario -> Binario -> Binario
aplicar Soma2 Um Um = Zero
aplicar Soma2 Zero Um = Um
aplicar Soma2 Um Zero = Um
aplicar Soma2 Zero Zero = Zero
aplicar Maior Um Zero = Um
aplicar Maior Zero Um = Um
aplicar Maior Um Um = Um
aplicar Maior Zero Zero = Zero
aplicar Menor Um Zero = Zero
aplicar Menor Zero Um = Zero
aplicar Menor Um Um = Um
aplicar Menor Zero Zero = Zero
aplicar Mult2 Um Um= Um
aplicar Mult2 Zero Um = Zero
aplicar Mult2 Um Zero = Zero
aplicar Mult2 Zero Zero = Zero


-- 4.1
mediaDoubles :: [Double] -> Double
mediaDoubles xs = ((foldl (+)  (head xs) (tail xs))/ fromIntegral (length xs))

-- 4.3
filtrarPar :: [Int] -> [Int]
filtrarPar xs = filter (odd) xs

filtrarImpar :: [Int] -> [Int]
filtrarImpar xs = filter (even) xs

-- 4.4 Filtre	 os	 números	 primos	 de	 uma	 lista	 recebida	 por parâmetro.

-- 4.5
dobraSemQuatro :: [Int] -> [Int]
dobraSemQuatro xs = map (*2) (filter (\x -> mod x 4 /= 0) xs)

-- 4.6
func :: (String -> String) -> String -> String
func f s = (reverse s)++(f s)
-- 4.7
data Dia = Domingo | Segunda | Terca | Quarta | Quinta | Sexta | Sabado deriving (Eq, Show)
filtraTerca:: [Dia] -> [Dia]
filtraTerca xs = filter (\x -> x /= Terca) xs

-- 4.8 
-- data Correncia = Real | Dolar deriving (Show, Eq)
-- data Dinheiro = Dinheiro {valor :: Double,
--                            curr :: Correncia} deriving (Show, Eq)
-- converteDolar :: Dinheiro -> Dinheiro
-- converteDolar (Dinheiro x y) 
--     |y == Real = (Dinheiro (x/3.2) Dolar) 
--     |otherwise = (Dinheiro x y)
    
-- converteReal :: Dinheiro -> Dinheiro
-- converteReal (Dinheiro x y)
--    |y == Dolar = (Dinheiro (x*3.2) Real)
--    |otherwise (Dinheiro x y)
    
-- 4.9 
contNeg :: [Int] -> Int
contNeg xs = foldl (\x y -> if y<0 then x+1 else x) 0 xs  

letrasP :: String -> Int
letrasP xs = foldl (\x y -> if y=='P' then x+1 else x)  0 xs 

contaSabado :: [Dia] -> Int
contaSabado xs = foldl (\x y -> if y==Sabado then x+1 else x) 0 xs

diaInt :: Dia -> Int
diaInt Domingo = 1
diaInt Segunda = 2
diaInt Terca = 3
diaInt Quarta = 4
diaInt Quinta = 5
diaInt Sexta = 6
diaInt Sabado = 7

somaDias :: [Dia] -> Int
somaDias xs = foldl (\x y -> x + (diaInt y)) 0 xs

-- 5.1
data TipoProduto = Escritorio | Informatica | Livro | Filme | Total deriving (Show, Eq)
data Produto = Produto {valor :: Double
                       ,tp :: TipoProduto} | Nada deriving (Show, Eq)


instance Monoid Produto where
    mempty = Nada
    mappend (Produto a _) (Produto b _) = (Produto (a+b) Total)
    mappend Nada (Produto a b) = Produto a Total
    mappend (Produto a b) Nada = Produto a Total
    mappend Nada Nada = Nada

-- valorTotal :: [Produto] -> Double
-- valorTotal (x:xs)
--                |x /= Nada = valor (x) + valorTotal xs 
--                |otherwise = valorTotal xs
-- valorTotal [] = 0

-- 5.2

totalGeral :: [Produto] -> Produto
totalGeral (x:[]) = x <> Nada
totalGeral (x:xs) = foldl (<>) x xs
totalGeral [] = Nada

-- 5.3 
data Min = Min (Int) deriving (Eq, Show, Ord)

instance Monoid Min where
    mempty = (maxBound::Int)
    mappend (Min a) (Min b) = Min (min a b)
    
    
-- 5.4
minAll:: [Min]-> Min
minAll (x:[]) = x
minAll (x:xs) = foldl (<>) x xs
minall [] = null

-- 6.1

--a partir daqui sao ex da prova

data GF4 = U | A | B -- | C 

instance Monoid GF4 where 
    mempty = U
    mappend A A = B
    mappend B B = A
    mappend A B = U
    mappend B A = U
    mappend U x = x
    mappend x U = x

--    
-- (\x -> x /= mempty) :: Monoid a => a -> Bool
-- (\x -> reverse x ++ x) :: [a] -> [a]
-- (\x -> odd.length x && length x >7) :: [a] -> Bool
-- (\x -> min x 0)  :: Int -> Int

-- Monoid -> Maybe String
-- Bool -> Maybe Bool

somaEhPar :: [Int] -> Bool
somaEhPar [] = True
somaEhPar xs = even (foldl (+) 0 xs) 

eNegativo :: Maybe Int -> Maybe Bool
eNegativo (Just x) = Just( x >= 0 )
eNegativo Nothing = Nothing

filtraMempty :: (Monoid a, Eq a) => [a] -> [a]
filtraMempty xs = filter ((/=) mempty) xs

sqrVet :: [Int] -> Int
--sqrVet xs = [x= (x1*x1) +x | x1 <-xs]
--sqrVet xs = foldl (flip(+).(^2)) 0 xs
sqrVet xs = foldl (\x y -> x+y^2) 0 xs

data Tripla a = Tripla a Int a deriving Show
instance Functor Tripla where
    fmap f (Tripla x y z) = Tripla (f x) (id y) ((id . f . id) z)
    
homs :: Tripla a -> (a,Int,a)
homs (Tripla x y z)= (x,y,z)

data EstadoLampada = Aceso | Apagado deriving Eq
click :: EstadoLampada -> EstadoLampada
click Aceso = Apagado
click Apagado = Aceso

filtApag :: [EstadoLampada] -> [EstadoLampada]
filtApag xs = filter ((/=) Apagado) xs

clickLista :: [EstadoLampada] -> [EstadoLampada]
clickLista xs = map click xs

instance Show EstadoLampada where
    show Aceso = "A lampada esta acesa"
    show Apagado = "A lampada esta apagada"
    
f1 :: (a,()) -> a
f1 x = fst x

f2 :: (b->a,b->c)->b->(a,c)
f2 x = \b -> ((fst x) b ,(snd x) b)

f3 :: (a,b)->a->b
f3 g a = g a

f4 :: (a,b,c,d,e,f,g) -> d 
f4 (-,-,-,d,-,-,-) = d

f5 :: (a->b->c) ->[a]->[b]->[c]
f5 g as bs = [ g a b | a <- as, b <- bs]