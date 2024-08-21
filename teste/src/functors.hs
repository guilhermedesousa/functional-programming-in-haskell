{-# LANGUAGE DeriveFunctor #-}

-- exemplo com lista

result :: [Int]
result = fmap (*2) [1, 2, 3] -- Resultado: [2, 4, 6]

-- exemplo com árvore

data Tree a = Leaf a | Node (Tree a) a (Tree a)
    deriving (Show, Functor)

{-
instance Functor Tree where
    fmap g (Leaf x)     = Leaf (g x)
    fmap g (Node l x r) = Node (fmap g l) (g x) (fmap g r)
-}

exampleTree :: Tree Int
exampleTree = (Node (Leaf 1) 2 (Node (Leaf 3) 4 (Leaf 5)))

resultTree :: Tree Int
resultTree = (*2) <$> exampleTree -- Resultado: Node (Leaf 2) (Node (Leaf 4) (Leaf 6))

-- Definindo Functors automaticamente

data Const b a = Const b

instance Functor (Const b) where
  fmap _ (Const x) = Const x

data Identity a = Identity a

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

data NadaOuUm a = Lft (Const () a) | Rgt (Identity a)

instance Functor NadaOuUm where
    fmap f (Lft x) = Lft (fmap f x)
    fmap f (Rgt x) = Rgt (fmap f x)

