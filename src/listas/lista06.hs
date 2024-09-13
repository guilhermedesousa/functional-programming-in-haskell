-- Ex. 01

newtype Caixa a = Caixa a deriving (Eq, Show)

instance Functor Caixa where
    fmap g (Caixa x) = Caixa (g x)

instance Applicative Caixa where
    pure                    = Caixa
    (Caixa f) <*> (Caixa x) = Caixa (f x)

instance Monad Caixa where
    return          = pure
    (Caixa x) >>= f = f x

-- Ex. 02

data Expr a = Var a | Val Int | Add (Expr a) (Expr a) deriving (Show)

instance Functor Expr where
    fmap g (Var x)     = Var (g x)
    fmap _ (Val n)     = Val n
    fmap g (Add e1 e2) = Add (fmap g e1) (fmap g e2)

instance Applicative Expr where
    pure          = Var
    (Var f) <*> e = fmap f e

instance Monad Expr where
    return            = pure
    (Var x) >>= f     = f x
    (Val n) >>= _     = Val n
    (Add e1 e2) >>= f = Add (e1 >>= f) (e2 >>= f)

-- Ex. 03

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

instance Applicative Identity where
    pure               = Identity
    (Identity f) <*> x = fmap f x

instance Monad Identity where
    return              = pure
    (Identity ma) >>= f = f ma

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x                    = Pair x x
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

instance Monad Pair where
    return             = pure
    (Pair ma mb) >>= f = f ma

-- Ex. 04

data Fantasma a = Fantasma

instance Functor Fantasma where
    fmap _ _ = Fantasma

instance Applicative Fantasma where
    pure _  = Fantasma
    _ <*> _ = Fantasma

instance Monad Fantasma where
    return         = pure
    Fantasma >>= _ = Fantasma

-- Ex. 05

data Duo a = Duo (Bool -> a)

instance Functor Duo where
    fmap f (Duo g) = Duo (f . g)

instance Applicative Duo where
    pure v              = Duo $ const v
    (Duo f) <*> (Duo g) = Duo $ \b -> f b (g b)

runDuo :: Duo a -> Bool -> a
runDuo (Duo f) = f

instance Monad Duo where
    return   = pure
    mf >>= g = Duo $ \b -> runDuo (g $ runDuo mf b) b

-- Ex. 06

data Request a = Loading | Error | Success a

instance Functor Request where
    fmap _ Loading     = Loading
    fmap _ Error       = Error
    fmap g (Success x) = Success (g x)

instance Applicative Request where
    pure                    = Success
    Error <*> _             = Error
    _ <*> Error             = Error
    Loading <*> _           = Loading
    _ <*> Loading           = Loading
    Success g <*> Success x = Success (g x)

instance Monad Request where
    return            = pure
    Error >>= _       = Error
    Loading >>= _     = Loading
    (Success x) >>= f = f x

-- Ex. 07

data Bolso a = Um a | Dois a a | Tres a a a

instance Functor Bolso where
    fmap f (Um x)       = Um (f x)
    fmap f (Dois x y)   = Dois (f x) (f y)
    fmap f (Tres x y z) = Tres (f x) (f y) (f z)

instance Eq a => Eq (Bolso a) where
    (Um x) == (Um y)             = x == y
    (Um x) == (Dois _ y)         = x == y
    (Um x) == (Tres _ _ y)       = x == y
    (Dois _ x) == (Dois _ y)     = x == y
    (Dois _ x) == (Um y)         = x == y
    (Dois _ x) == (Tres _ _ y)   = x == y
    (Tres _ _ x) == (Tres _ _ y) = x == y
    (Tres _ _ x) == (Um y)       = x == y
    (Tres _ _ x) == (Dois _ y)   = x == y

instance Applicative Bolso where
    pure               = Um
    (Um f) <*> x       = f <$> x
    (Dois _ f) <*> x   = f <$> x
    (Tres _ _ f) <*> x = f <$> x
    
instance Monad Bolso where
    return             = pure
    (Um x) >>= f       = f x
    (Dois _ x) >>= f   = f x
    (Tres _ _ x) >>= f = f x

-- Ex. 08

type Nome = String
type Peso = Double
type Altura = Double
type IMC = Double

imc :: [(Maybe Nome, Maybe Peso, Maybe Altura)] -> [Maybe IMC]
imc xs = fmap calculate xs
    where
        calculate (_, mp, ma) = do
            p <- mp
            a <- ma
            return $ p / (a ** 2)

-- Ex. 09

-- join: remove um nível de uma estrutura monádica
azul :: Monad m => m (m a) -> m a
azul m = m >>= id

-- Ex. 10

-- fmap: aplica uma função a cada elemento dentro de uma estrutura monádica (ou Functor)
amarelo :: Monad m => (a -> b) -> m a -> m b
amarelo = fmap

-- Ex. 11

-- liftM2 ou liftA2: aplica uma função binária a dois valores monádicos, combinando-os
vermelho :: Monad m => (a -> b -> c) -> m a -> m b -> m c
vermelho f ma mb = f <$> ma <*> mb

-- Ex. 12

-- ap: aplica uma função monádica a um valor monádico
verde :: Monad m => m a -> m (a -> b) -> m b
verde ma mf = mf <*> ma

-- Ex. 13

-- sequence: toma uma lista de monadas e as "sequencia" em uma única monada
laranja :: Monad m => [m a] -> m [a]
laranja [] = return []
laranja (x:xs) = do
    x1 <- x
    xs' <- laranja xs
    return $ x1 : xs'

-- Ex. 14

-- mapM: aplica uma função monádica a cada elemento de uma lista e depois sequencia
roxo :: Monad m => [a] -> (a -> m b) -> m [b]
roxo xs f = laranja $ amarelo f xs

-- Ex. 16

newtype MudaLista a = MudaLista { runMudaLista :: [Int] -> ([Int], a)}

desempilha :: MudaLista Int
desempilha = MudaLista $ \(x:xs) -> (xs, x)

empilha :: Int -> MudaLista ()
empilha x = MudaLista $ \l -> (x:l, ())

instance Functor MudaLista where
    fmap g (MudaLista f) = MudaLista $ \l -> fmap g (f l)

-- instance Applicative MudaLista where
--     pure x = MudaLista $ \l -> (l, x)
--     (MudaLista fx) <*> (MudaLista f) =
--         MudaLista $ \l ->
--             let (l1, fab) = fx l
--                 (l2, a) = f l1
--             in (l2, fab a)

-- desempilhaVarios :: Int -> MudaLista [Int]
-- desempilhaVarios n = sequenceA (replicate n desempilha)

-- empilhaVarios :: [Int] -> MudaLista ()
-- empilhaVarios = traverse_ empilha