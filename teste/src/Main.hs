module Main (main) where

tamanho :: [a] -> Int
tamanho [] = 0
tamanho (_:xs) = 1 + tamanho xs

impar :: Integral a => a -> Bool
impar n = n `mod` 2 == 1

quadrado :: Num a => a -> a
quadrado x = x * x

quadradoMais6Mod9 :: Integral a => a -> a
quadradoMais6Mod9 x = (x * x + 6) `mod` 9

raiz2Grau :: Floating a => a -> a -> a -> (a, a)
raiz2Grau a b c = (((-b) + sqrt (b^(2 :: Int) - 4*a*c)) / (2*a),((-b) - sqrt (b^(2 :: Int) - 4*a*c)) / (2*a))

raiz2GrauV2 :: Floating a => a -> a -> a -> (a, a)
raiz2GrauV2 a b c = (x1, x2)
  where
    x1 = ((-b) + sqDelta) / (2*a)
    x2 = ((-b) - sqDelta) / (2*a)
    sqDelta = sqrt delta
    delta = b^(2 :: Int) - 4*a*c

myAbs :: (Num a, Ord a) => a -> a
myAbs n = if n >= 0 then n else (-n)

mySignum :: (Num a, Ord a) => a -> a
mySignum n = if (n == 0)
           then 0
           else if (n > 0)
             then 1
             else (-1)

raiz2GrauV3 :: (Floating a, Ord a) => a -> a -> a -> (a, a)
raiz2GrauV3 a b c = (x1, x2)
  where
    x1 = if delta > 0
         then ((-b) + sqDelta) / (2*a)
         else 0
    x2 = if delta > 0
         then ((-b) - sqDelta) / (2*a)
         else 0
    sqDelta = sqrt delta
    delta = b^(2 :: Int) - 4*a*c

mySignum2 :: (Num a, Ord a) => a -> a
mySignum2 n | n == 0    =  0
            | n > 0     =  1
            | otherwise = -1

classificaIMC :: Double -> String
classificaIMC imc
  | imc <= 18.5 = "abaixo do peso"
  | imc <= 25.0 = "no peso correto"
  | imc <= 30.0 = "acima do peso"
  | otherwise   = "muito acima do peso"

raiz2GrauV4 :: (Ord a, Floating a) => a -> a -> a -> (a, a)
raiz2GrauV4 a b c
  | delta >= 0 = (x1, x2)
  | otherwise  = error "Delta negativo"
  where
    x1 = if delta > 0
         then ((-b) + sqDelta) / (2*a)
         else 0
    x2 = if delta > 0
         then ((-b) - sqDelta) / (2*a)
         else 0
    sqDelta = sqrt delta
    delta = b^(2 :: Int) - 4*a*c

not2 :: Bool -> Bool
not2 x = if x then False else True

not3 :: Bool -> Bool
not3 x | x == True = False
       | x == False = True

not4 :: Bool -> Bool
not4 True  = False
not4 False = True

soma2 :: (Eq a, Num a) => a -> a -> a
soma2 x 0 = x
soma2 0 y = y
soma2 x y = x + y

mul2 :: (Eq a, Num a) => a -> a -> a
mul2 x 0 = 0
mul2 0 y = 0
mul2 x 1 = x
mul2 1 y = y
mul2 x y = x*y

mul3 :: (Eq a, Num a) => a -> a -> a
mul3 _ 0 = 0
mul3 0 _ = 0
mul3 x 1 = x
mul3 1 y = y
mul3 x y = x*y

somaMultX :: Num a => a -> (a -> a)
somaMultX x = \y -> x + x * y

somaMult2 = somaMultX 2

(&&&) :: Bool -> Bool -> Bool
True &&& True = True
_    &&& _    = False

-- LISTAS

lista = 1 : 2 : 3 : 4 : []

myTake :: Int -> [a] -> [a]
myTake n _ | n <= 0 = []
myTake _ []         = []
myTake n (x:xs)     = x : myTake (n-1) xs

-- COMPREENSÃO DE LISTAS

divisores :: Int -> [Int]
divisores n = [x | x <- [1..n], n `mod` x == 0]

primo :: Int -> Bool
primo n = divisores n == [1,n]

todosOsPrimos = [x | x <- [1..], primo x]

nEsimoPrimo :: Int -> Int
nEsimoPrimo n = todosOsPrimos !! (n-1)

-- RECURSÃO

fatorial :: Integer -> Integer
fatorial 0 = 1
fatorial 1 = 1
fatorial n = n * fatorial (n-1)

mdc :: Int -> Int -> Int
mdc 0 b = b
mdc a 0 = a
mdc a b = mdc b (a `rem` b)

somaLista :: Num a => [a] -> a
somaLista [] = 0
somaLista (n:ns) = n + somaLista ns

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort menores ++ [x] ++ qsort maiores
  where
    menores = [e | e <- xs, e < x]
    maiores = [e | e <- xs, e >= x]

-- FUNÇÕES DE ALTA ORDEM

soma :: Int -> (Int -> Int)
soma x y = x+y

soma3 = soma 3

dobra = (*2)
metade = (/2)
reciproco = (1/)

duasVezes :: (a -> a) -> a -> a
duasVezes f x = f (f x)

quadruplica = duasVezes (*2)

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

map'' :: (a -> b) -> [a] -> [b]
map'' f [] = []
map'' f (x:xs) = f x : map'' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

somaQuadPares :: [Int] -> Int
somaQuadPares ns = sum
                $ map (^2)
                $ filter even ns

-- FOLDING

tamanho' :: [a] -> Int
tamanho' = foldr (\_ n -> n+1) 0

main :: IO ()
main = do
  putStrLn "hello world"
