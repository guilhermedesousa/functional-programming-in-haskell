-- Ex. 01

data Nat = Zero | Succ Nat
            deriving (Show)

add :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (add m n)

mult :: Nat -> Nat -> Nat
mult Zero _ = Zero
mult (Succ m) n = add n (mult m n)

-- Ex. 02

data Tree a = Leaf a | Node (Tree a) a (Tree a)
            deriving (Show)

occurs :: Ord a => a -> Tree a -> Bool
occurs x (Leaf y) = x == y
occurs x (Node left y right) =
    case (compare x y) of
        LT -> occurs x left
        EQ -> True
        GT -> occurs x right

-- Ex. 03

flatten :: Tree a -> [a]
flatten (Leaf y) = [y]
flatten (Node left y right) = flatten left ++ [y] ++ flatten right

-- Ex. 04

countLeaves :: Tree a -> Int
countLeaves (Leaf _) = 1
countLeaves (Node left _ right) = countLeaves left + countLeaves right

balanced :: Tree a -> Bool
balanced (Leaf _) = True
balanced (Node left _ right)
    | abs(countLeaves left - countLeaves right) <= 1 && balanced left && balanced right = True
    | otherwise = False

-- Ex. 05

splitList :: [a] -> ([a], [a])
splitList xs = splitAt (length xs `div` 2) xs

balance :: [a] -> Tree a
balance [x] = Leaf x
balance (x:xs) = Node (balance left) x (balance right)
    where
        left  = fst $ splitList xs
        right = snd $ splitList xs

-- Ex. 06

data Expr = Val Int | Add Expr Expr
        deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g (Val x) = f x
folde f g (Add expr1 expr2) = g (folde f g expr1) (folde f g expr2)

-- Ex. 07

eval :: Expr -> Int
eval expr = folde id (+) expr

size :: Expr -> Int
size expr = folde (\_ -> 1) (+) expr

-- Ex. 08

data List a = Nil | a :- List a
    deriving (Show)
infixr 5 :-

data Sem = Green | Yellow | Red
    deriving (Eq, Show)

count :: Sem -> List Sem -> Int
count _ Nil = 0
count x (y :- ys)
    | x == y = 1 + count x ys
    | otherwise = count x ys

next :: Sem -> Sem
next Green  = Yellow
next Yellow = Red
next Red    = Green

-- Ex. 09

{-
    1u para atravessar um sem
    1u para o sem mudar de cor
    1u para a Red -> Green
-}

mapl :: (a -> b) -> List a -> List b
mapl _ Nil       = Nil
mapl f (x :- xs) = f x :- mapl f xs

timeList :: List Sem -> Int
timeList Nil       = 0
timeList (x :- xs) = 1 + timeList (mapl next xs2)
    where
        xs2 = if x == Red then x :- xs else xs

-- Ex. 10

redl :: (b -> a -> b) -> b -> List a -> b
redl f v Nil       = v
redl f v (x :- xs) = redl f (f v x) xs

-- Ex. 11

timeListRedl :: List Sem -> Int
timeListRedl = fst . redl foldFun (0, id)
    where
        foldFun (n, f) c
            | f c == Red = (n + 2, next . next . f)
            | otherwise  = (n + 1, next . f)

data BT a = BEmpty | BNode a (BT a) (BT a)
    deriving (Show)

bleaf :: a -> BT a
bleaf x = BNode x BEmpty BEmpty

bt :: BT Sem
bt = BNode Green
    (
        BNode Green (bleaf Red) (bleaf Green)
    )
    (
        BNode Yellow (bleaf Red) (bleaf Green)
    )

-- Ex. 12

-- fold associativo a esquerda que toma o primeiro elemento da
-- lista como acumulador inicial
redl1 :: (a -> a -> a) -> List a -> a
redl1 f (x :- xs) = redl f x xs

-- devolve o elemento minimo de uma lista
minimuml :: Ord a => List a -> a
minimuml = redl1 min

-- dada uma arvore binaria de a, devolve todos os caminhos das folhas
-- a raiz. Para evitar concatenacoes de listas, esta implementacao
-- utiliza uma variavel acumuladora o que acaba aumentando
-- ligeiramente a complexidade da solucao.
allPaths :: BT a -> List (List a)
allPaths = go (Nil, False, Nil)
    where
        -- (atual, incluir, caminhos acumulados)
        go :: (List a, Bool, List (List a)) -> BT a -> List (List a)
        go (curr, True, ps) BEmpty = curr :- ps
        go (_, False, ps) BEmpty = ps
        go (curr, _, ps) (BNode x l r) = psr
            where
                -- para evitar repeticoes, so adiciona uma das folhas
                -- terminadoras
                psl = go (x :- curr, True, ps) l
                psr = go (x :- curr, False, psl) r

timeBT :: BT Sem -> Int
timeBT = minimuml . mapl timeListRedl . allPaths

-- Ex. 13

-- devolve a cabeça da lista passada como parâmetro
headl :: List a -> a
headl (x :- _) = x

-- dado um par de listas, devolve uma lista de pares contendo os
-- valores das listas recebidas
zipl :: List a -> List b -> List (a, b)
zipl Nil _ = Nil
zipl _ Nil = Nil
zipl (x :- xs) (y :- ys) = (x, y) :- zipl xs ys

-- filtra os elementos da lista recebida como parametro de acordo com
-- o predicado fornecido
filterl :: (a -> Bool) -> List a -> List a
filterl _ Nil = Nil
filterl p (x :- xs)
    | p x       = x :- filterl p xs
    | otherwise = filterl p xs

-- concatena duas listas
appendl :: List a -> List a -> List a
appendl xs ys = redl (flip (:-)) ys xs

-- ordena uma lista utilizando como criterio a funcao de comparacao
-- fornecida
sortByl :: (a -> a -> Bool) -> List a -> List a
sortByl _ Nil = Nil
sortByl f (x :- xs) = appendl menores (x :- maiores)
    where
        menores = filterl (`f` x) xs
        maiores = filterl (\a -> not $ f a x) xs

bestBT :: BT Sem -> List Sem
bestBT xs = snd . headl . sortByl (\(t0,_) (t1,_) -> t0 <= t1) $ zipl ts ps
    where
        ps = allPaths xs
        ts = mapl timeListRedl ps

data Tree a = TEmpty | TNode a (List (Tree a))

tleaf :: a -> Tree a
tleaf x = TNode x Nil

tree :: Tree Sem
tree = TNode Green
    ((TNode Green (tleaf Red :- tleaf Green :- Nil)) :-
    (TNode Yellow
    (TNode Red (tleaf Red :- tleaf Green :- Nil) :- tleaf Yellow :- Nil)) :-
    (TNode Green (tleaf Green :- tleaf Green :- Nil)) :- Nil)