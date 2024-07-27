module Main (main) where

-- ex 4
palidromo :: (Eq a) => [a] -> Bool
palidromo xs = xs == reverse xs

-- ex 11
penultimo :: [a] -> a
penultimo [] = error "Lista vazia!"
penultimo [_] = error "Lista com apenas um elemento!"
penultimo [a, _] = a
penultimo (_:xs) = penultimo xs

-- ex 12
maximoLocal :: [Int] -> [Int]
maximoLocal [] = []
maximoLocal [_] = []
maximoLocal [_, _] = []
maximoLocal (x:y:z:xs)
  | y > x && y > z = y : maximoLocal (z:xs)
  | otherwise = maximoLocal (y:z:xs)

-- ex 13
perfeito :: Int -> Bool
perfeito n = somaFatores == n
  where
    somaFatores = sum divisoresProprios
    divisoresProprios = [x | x <- [1..(n-1)], n `mod` x == 0]

perfeitos :: Int -> [Int]
perfeitos n = [x | x <- [1..n], perfeito x]

-- ex 14
produtoEscalar :: Num a => [a] -> [a] -> a
produtoEscalar xs ys = sum [x*y | (x,y) <- zip xs ys]

-- ex 15
palindromo :: [Int] -> Bool
palindromo [] = True
palindromo [_] = True
palindromo xs = head xs == last xs && palindromo (init (tail xs))

-- ex 16
ordernaListas :: (Num a, Ord a) => [[a]] -> [[a]]
ordernaListas [] = []
ordernaListas (xs:xss) = ordernaListas menores ++ [xs] ++ ordernaListas maiores
  where
    menores = [as | as <- xss, length as <= length xs]
    maiores = [bs | bs <- xss, length bs > length xs]

-- ex 17
coord :: [a] -> [a] -> [(a,a)]
coord x y = concat [[(i,j) | i <- x] | j <- y]

-- ex 18
-- (a)
digitosRev :: Int -> [Int]
digitosRev 0 = []
digitosRev n = n `mod` 10 : digitosRev (n `div` 10)

-- (b)
dobroAlternado :: [Int] -> [Int]
dobroAlternado []       = []
dobroAlternado (x:y:xs) = x : y*2 : dobroAlternado xs

-- (c)
somaDigitos :: [Int] -> Int
somaDigitos []     = 0
somaDigitos (x:xs) = sum (digitosRev x) + somaDigitos xs

-- (d)
luhn :: Int -> Bool
luhn n = somaDigitos (dobroAlternado (digitosRev n)) `mod` 10 == 0

-- ex 19
mc91 :: Integral a => a -> a
mc91 n
  | n > 100   = n - 10
  | otherwise = mc91 (mc91 (n + 11))

-- ex 20
elem' :: Eq a => a -> [a] -> Bool
elem' _ []      = False
elem' x' (x:xs) = if x' == x then True else elem' x' xs

-- ex 21
euclid :: Int -> Int -> Int
euclid x y
  | x == y    = x
  | x > y     = euclid (x-y) y
  | otherwise = euclid x (y-x)

-- ex 22
concat' :: [[a]] -> [a]
concat' []       = []
concat' (xs:xss) = xs ++ concat' xss

-- ex 23
intersperse' :: a -> [a] -> [a]
intersperse' _ []     = []
intersperse' _ [c]    = [c]
intersperse' s (c:cs) = c : s : intersperse' s cs

-- ex 24
digitsToText :: [Int] -> [[Char]]
digitsToText []     = []
digitsToText (x:xs) = digitToText x : digitsToText xs
  where
    digitToText d = ["zero","um","dois","tres","quatro","cinco","seis","sete","oito","nove"] !! d

digits :: Int -> [Int]
digits 0 = [0]
digits n = reverse (digits' n)
  where
    digits' 0 = []
    digits' x = (x `mod` 10) : digits' (x `div` 10)

intersperse'' :: a -> [[a]] -> [[a]]
intersperse'' _ []       = []
intersperse'' _ [c]      = [c]
intersperse'' s (xs:xss) = [xs] ++ [s] : intersperse'' s xss

wordNumber :: Char -> Int -> [Char]
wordNumber ch n = concat' (intersperse'' ch (digitsToText (digits n)))

-- ex 25
merge :: Ord a => [a] -> [a] -> [a]
merge xs []         = xs
merge [] ys         = ys
merge (x:xs) (y:ys)
  | x > y     = y : merge (x:xs) ys
  | otherwise = x : merge xs (y:ys)

-- ex 26
halve :: [a] -> ([a], [a])
halve xs  = (xs1, xs2)
  where
    xs1 = take half xs
    xs2 = drop half xs
    half = (length xs) `div` 2

msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort m1) (msort m2)
  where
    m1 = fst (halve xs)
    m2 = snd (halve xs)

-- ex 27
quotRem' :: Integral a => a -> a -> (a, a)
quotRem' _ 0 = error "Divisão por zero!"
quotRem' n d
  | n == 0         = (0 ,0)
  | n > 0 && d > 0 = positiveQuotRem n d
  | n < 0 && d < 0 = positiveQuotRem (-n) (-d)
  | n < 0 && d > 0 = let (q, r) = positiveQuotRem (-n) d in (-q, -r)
  | n > 0 && d < 0 = let (q, r) = positiveQuotRem n (-d) in (-q, r)
  where
    positiveQuotRem x y
      | x < y     = (0, x)
      | otherwise = let (q, r) = positiveQuotRem (x - y) y in (q + 1, r)

divMod' :: Integral a => a -> a -> (a, a)
divMod' _ 0 = error "Divisão por zero!"
divMod' n d
  | n == 0    = (0, 0)
  | n * d > 0 = quotRem' n d
  | otherwise = let (q, r) = quotRem' n d
                in if r == 0
                  then (q, r)
                  else (q - 1, r + d)

-- ex 28
skipN :: [a] -> Int -> [a]
skipN xs n = [xs !! i | i <- [n-1, (n-1+n)..length xs - 1]]

skips :: [a] -> [[a]]
skips [] = []
skips xs = [skipN xs i | i <- [1..length xs]]

-- ex 29
all' :: (a -> Bool) -> [a] -> Bool
all' _ [] = True
all' p (x:xs) = p x && all' p xs

any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' p (x:xs) = p x || any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x:xs)
  | p x       = x : takeWhile' p xs
  | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' _ [] = []
dropWhile' p (x:xs)
  | p x       = dropWhile' p xs
  | otherwise = x : xs

-- ex 30
altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap f g (x:xs) = f x : altMap g f xs

-- ex 31
dec2int :: [Int] -> Int
dec2int = foldl (\acc x -> acc * 10 + x) 0
-- (((0 `f` 2) `f` 3) `f` 4) `f` 5
-- ((2 `f` 3) `f` 4) `f` 5
-- (23 `f` 4) `f` 5
-- 234 `f` 5
-- 2345

-- ex 32
or' :: [Bool] -> Bool
or' = foldr (||) False

reverse' :: [a] -> [a]
reverse' = foldl (\xs x -> x:xs) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x:acc else acc ) []

-- ex 33
elem'' :: Eq a => a -> [a] -> Bool
elem'' x = foldr (\y acc -> (x == y) || acc) False

elem''' :: Eq a => a -> [a] -> Bool
elem''' x = any (== x)

main :: IO ()
main = do
  putStrLn "hello world"
