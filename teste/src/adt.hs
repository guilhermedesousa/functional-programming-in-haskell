type Produto = (Integer, String, Double)
type Cliente = (Integer, String, Double)

preco :: Produto -> Double
preco (_, _, p) = p

pago :: Cliente -> Double
pago (_, _, p) = p

troco :: Produto -> Cliente -> Double
troco produto cliente = pago cliente - preco produto

type Assoc k v = [(k,v)]

find :: Eq k => k -> Assoc k v -> v
find k t = head [v | (k', v) <- t, k == k']

-- ADT

-- Tipo Soma

data Dir = Norte | Sul | Leste | Oeste
            deriving Show

type Coord = (Int, Int)
type Passo = Coord -> Coord

para :: Dir -> Passo
para Norte (x,y) = (x, y + 1)
para Sul   (x,y) = (x, y - 1)
para Leste (x,y) = (x + 1, y)
para Oeste (x,y) = (x - 1, y)

caminhar :: [Dir] -> Passo
caminhar []     coord = coord
caminhar (d:ds) coord = caminhar ds (para d coord)

data Ponto = MkPonto Double Double
            deriving Show

dist :: Ponto -> Ponto -> Double
dist (MkPonto x y) (MkPonto x' y') = sqrt $ (x-x')^2 + (y-y')^2

data Forma = Circulo Ponto Double
           | Retangulo Ponto Double Double

-- um quadrado é um retângulo com os dois lados iguais
quadrado :: Ponto -> Double -> Forma
quadrado p n = Retangulo p n n

-- Exercicio 1

data Fuzzy = Verdadeiro | Falso | Pertinencia Double
                deriving Show

fuzzifica :: Double -> Fuzzy
fuzzifica x
    | x <= 0    = Falso
    | x >= 1    = Verdadeiro
    | otherwise = Pertinencia x

-- Tipos Recursivos

data Lista a = Vazia | Cons a (Lista a)

data Tree a = Leaf a
            | Node {
                _right :: (Tree a),
                _value :: a,
                _left  :: (Tree a)
            } deriving (Show)

t :: Tree Int
t = Node (Node (Leaf 1) 3 (Leaf 4)) 5
       (Node (Leaf 6) 7 (Leaf 9))

contem :: Eq a => Tree a -> a -> Bool
contem (Leaf y) x     = x == y
contem (Node l y r) x = x == y || l `contem` x || r `contem` x

contem2 :: Ord a => Tree a -> a -> Bool
contem2 (Leaf y) x    = x == y
contem2 (Node l y r) x
    | x == y    = True
    | x < y     = l `contem` x
    | otherwise = r `contem` x