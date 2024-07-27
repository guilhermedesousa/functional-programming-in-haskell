bools :: [Bool]
bools = [True, False, True]

nums :: [[Int]]
nums = [[1,2],[3,4]]

soma :: Int -> Int -> Int -> Int
soma a b c = a + b + c

copia :: a -> (a, a)
copia x = (x, x)

f :: a -> a
f x = x

g :: Eq a => a -> (a, a) -> Bool
g x (y,z) = x == y || x == z

h :: Num a => Int -> a -> a
h x y = fromIntegral x * y