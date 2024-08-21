import Data.List as L (intercalate, nub, sort, delete)
import Text.Read
import Data.Monoid

-- Ex. 01

data Resultado = Pontuacao Int | Cola

instance Semigroup Resultado where
    (Pontuacao x) <> (Pontuacao y) = Pontuacao (x+y)
    _ <> _                         = Cola

instance Monoid Resultado where
    mempty = Pontuacao 0

-- Ex. 02

data Set a = Set [a] deriving Eq

instance Show a => Show (Set a) where
    show (Set xs) = "{" ++ intercalate "," (map show xs) ++ "}"

fromList :: Ord a => [a] -> Set a
fromList = Set . sort . nub -- Set (sort (nub xs))

member :: Ord a => a -> Set a -> Bool
member x (Set xs) = x `elem` xs

insert :: Ord a => a -> Set a -> Set a
insert x (Set xs) = fromList $ (x : xs)

delete :: Ord a => a -> Set a -> Set a
delete x (Set xs) = Set $ L.delete x xs

-- Ex. 03

instance Ord a => Semigroup (Set a) where
    (Set as) <> (Set bs) = fromList $ (as <> bs)

instance Ord a => Monoid (Set a) where
    mempty = Set []

-- Ex. 04

data Dieta = Vegano | Vegetariano | Tradicional

data Lanche = Lanche (Set String) Int Dieta

instance Semigroup Dieta where
    Tradicional <> _      = Tradicional
    _ <> Tradicional      = Tradicional
    Vegetariano <> _      = Vegetariano
    Vegano <> Vegetariano = Vegetariano
    Vegano <> Vegano      = Vegano

instance Monoid Dieta where
    mempty = Vegano

-- Ex. 05

instance Semigroup Lanche where
    (Lanche is1 p1 d1) <> (Lanche is2 p2 d2) = Lanche (is1 <> is2) (p1 + p2) (d1 <> d2)

instance Monoid Lanche where
    mempty = Lanche mempty 0 mempty

-- Ex. 06

data Tree a = Leaf a | Node (Tree a) a (Tree a) deriving Show

instance Functor Tree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node left x right) = Node (fmap f left) (f x) (fmap f right) 

-- Ex. 07

arvorePossui :: Eq a => Tree a -> a -> Bool
arvorePossui (Leaf v) x = x == v
arvorePossui (Node left v right) x =
    x == v ||
    arvorePossui left x ||
    arvorePossui right x

-- Ex. 08

contaLetras :: Tree String -> Tree Int
contaLetras = fmap (\x -> length x)

-- Ex. 09

instance Foldable Tree where
    foldMap f (Leaf x)     = f x
    foldMap f (Node l x r) = foldMap f l <> f x <> foldMap f r

    foldr f acc (Leaf x)     = f x acc
    foldr f acc (Node l x r) = foldr f (f x (foldr f acc r)) l

exampleTree :: Tree Int
exampleTree = Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5))

sumTree :: Tree Int -> Int
sumTree = foldr (+) 0

-- Ex. 10

convertString2Int :: String -> Maybe Int
convertString2Int = readMaybe

-- Ex. 11

nothingToZero :: Maybe Int -> Int
nothingToZero Nothing  = 0
nothingToZero (Just x) = x

-- Ex. 12

frutasDaArvore :: Tree String -> Int
frutasDaArvore = getSum . foldMap (Sum . nothingToZero . convertString2Int)

-- Ex. 13

newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> f a -> f b
    fmap g (Z xs) = Z (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> Z a
    pure x = Z (repeat x)
    -- <*> :: Z (a -> b) -> Z a -> Z b
    (Z gs) <*> (Z xs) = Z [g x | (g, x) <- zip gs xs]

-- Ex. 15

newtype Identity a = Identity a
data Pair a = Pair a a

instance Functor Identity where
    fmap g (Identity x) = Identity (g x)

instance Applicative Identity where
    -- pure :: a -> Identity a
    pure x = Identity x
    -- <*> :: Identity (a -> b) -> Identity x -> Identity b
    Identity g <*> x = fmap g x

instance Functor Pair where
    fmap g (Pair x y) = Pair (g x) (g y)

instance Applicative Pair where
    -- pure :: a -> Pair a a
    pure x = Pair x x
    -- <*> :: Pair (a -> b) -> Pair a -> Pair b
    (Pair g h) <*> (Pair x y) = Pair (g x) (h y)

-- Ex. 16

data RLE a = Repeat Int a (RLE a) | End deriving (Eq, Show)

rleCons :: Eq a => a -> RLE a -> RLE a
rleCons x End = Repeat 1 x End
rleCons x r@(Repeat n y rle)
    | x == y    = Repeat (n + 1) y rle
    | otherwise = Repeat 1 x r

-- Ex. 17

instance Foldable RLE where
    foldMap _ End = mempty
    foldMap f (Repeat i x xs) = foldMap f (replicate i x) <> foldMap f xs

-- Ex. 18

encode :: Eq a => [a] -> RLE a
encode = foldr rleCons End

-- Ex. 19

decode :: RLE a -> [a]
decode = foldMap (\x -> [x])

-- Ex. 20

data Fantasma a = Fantasma

instance Functor Fantasma where
    fmap _ _ = Fantasma

instance Applicative Fantasma where
    pure _  = Fantasma
    _ <*> _ = Fantasma

-- Ex. 21

data Duo a = Duo (Bool -> a)

instance Functor Duo where
    -- fmap :: (a -> b) -> Duo a -> Duo b
    fmap f (Duo g) = Duo (f . g)

instance Applicative Duo where
    -- pure :: a -> Duo a
    pure v = Duo $ const v
    -- <*> :: Duo (a -> b) -> Duo a -> Duo b
    (Duo f) <*> (Duo g) = Duo $ \b -> f b (g b)