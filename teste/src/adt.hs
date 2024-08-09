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

-- Tipo Produto

data Ponto = MkPonto Double Double
            deriving Show

dist :: Ponto -> Ponto -> Double
dist (MkPonto x y) (MkPonto x' y') = sqrt $ (x-x')^2 + (y-y')^2

data Forma = Circulo Ponto Double
           | Retangulo Ponto Double Double

-- um quadrado é um retângulo com os dois lados iguais
quadrado :: Ponto -> Double -> Forma
quadrado p n = Retangulo p n n

maybeDiv :: Int -> Int -> Maybe Int
maybeDiv _ 0 = Nothing
maybeDiv m n = Just (m `div` n)

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead xs = Just (head xs)

divComErro :: Int -> Int -> Int
divComErro m n = case (maybeDiv m n) of
                    Nothing -> error "divisao por 0"
                    Just x -> x

eitherDiv' :: Int -> Int -> Either String Int
eitherDiv' _ 0 = Left "divisao por 0"
eitherDiv' m n = Right (m `div` n)

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
    | x < y     = l `contem2` x
    | otherwise = r `contem2` x

-- Record Type

data Ponto3D = Ponto {
    coordX :: Double,
    coordY :: Double,
    coordZ :: Double
}

-- Algebra de Tipos

--data Zero -- Void
--data Um = ()

-- absurdo :: Void -> a
-- absurdo x = undefined

inteiro :: () -> Int
inteiro () = 10

var_inteiro :: Int
var_inteiro = 10

fim :: a -> ()
fim x = ()

-- type ZeroMaisUm = Either Void ()

type Bool' = Either () ()

bool2bool' False = Left ()
bool2bool' True = Right ()

type Dir' = Either (Either () ()) (Either () ())

norte, sul, leste, oeste :: Dir'
norte = Left (Left ())
sul   = Left (Right ())
leste = Right (Left ())
oeste = Right (Right ())

-- Zippers

data List a = Empty | Cons a (List a)