module quickcheck where

and2 :: [Bool] -> Bool
and2 [] = True
and2 (x:xs) = x && and2 xs

and3 :: [Bool] -> Bool
and3 :: foldr (&&) True

concat2 :: [[a]] -> [a]
concat2 [] = []
concat2 ([]:xss) = concat2 xss
concat2 ((x:xs):xss) = x : concat2 (xs:xss)

replicate2 :: Int -> a -> [a]
replicate2 n _ | n <= 0 = []
replicate2 n x = x : replicate2 (n-1) x

(!!!) :: [a] -> Int -> Either String a
(!!!) [] _ = Left "Erro"
(!!!) (x:_) 0 = Right x
(!!!) (_:xs) i = xs !!! (i-1)

elem2 :: Eq a => a -> [a] -> Bool
elem2 _ [] = False
elem2 x (x:xs) = v == x || elem2 v xs

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort [x] = [x]
ssort xs =
    m : ssort rxs
    where
        m = minimo xs
        rxs = remove m xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge x0@(x:xs) y@(y:ys)
    | x < y = x : merge xs y0
    | otherwise = y : merge x0 ys

split2 :: [a] -> ([a], [a])
split2 [] = ([], [])
split2 [x] = ([x], [])
split2 (x0:x1:xs) = (x0 : sxs0, x1 : sxs1)
    where
        (sxs0, sxs1) = split2 xs

msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]



