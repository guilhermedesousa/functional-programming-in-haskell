import Data.Monoid
import Control.Monad ((>=>))

-- Monads

data Expr = Val Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

{-
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)
-}

{-
-- partial function
eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = (eval x) + (eval y)
eval (Sub x y) = (eval x) - (eval y)
eval (Mul x y) = (eval x) * (eval y)
eval (Div x y) = (eval x) `div` (eval y)
-}

{-
eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = case eval x of
                    Nothing -> Nothing
                    Just n -> case eval y of
                        Nothing -> Nothing
                        Just m -> maybeDiv n m
-}

{-
eval :: Expr -> Maybe Int
eval (Val n)   = Just n
eval (Div x y) = eval x >>= \n ->
                 eval y >>= \m ->
                 safeDiv n m
-}

data SafeNum a = NaN | NegInf | PosInf | SafeNum a deriving (Show)

boxedCoerce :: SafeNum a -> SafeNum b
boxedCoerce NaN = NaN
boxedCoerce NegInf = NegInf
boxedCoerce PosInf = PosInf
boxedCoerce _ = error "Não deveria ser usado para valores safe"

flatten :: SafeNum (SafeNum a) -> SafeNum a
flatten (SafeNum sn) = sn
flatten v = boxedCoerce v

instance Functor SafeNum where
  fmap f (SafeNum n) = SafeNum $ f n
  fmap _ x = boxedCoerce x

instance Applicative SafeNum where
  pure = SafeNum
  f <*> x = flatten $ fmap (`fmap` x) f

instance Monad SafeNum where
  (SafeNum x) >>= f = f x
  x >>= _ = boxedCoerce x
  return = pure

-- Monads e Efeitos

data Dado = Um | Dois | Tres | Quatro | Cinco | Seis
               deriving (Show, Bounded, Enum, Eq)

jogaDado = [Um .. Seis]

{-
jogaDuasVezes = do
  dado1 <- jogaDado
  dado2 <- jogaDado
  return (dado1, dado2)
-}

magica :: Dado -> [Dado]
magica dado
  | any (dado==) [Um, Tres, Cinco] = [Dois, Quatro, Seis]
  | otherwise                      = [Um, Tres, Cinco]

{-
jogaDuasVezes' :: [(Dado, Dado)]
jogaDuasVezes' = do
  dado1 <- jogaDado
  dado2 <- magica dado1
  return (dado1, dado2)
-}

jogaDuasVezes :: [(Dado, Dado)]
jogaDuasVezes = jogaDado >>= \dado1 ->
                  jogaDado >>= \dado2 ->
                    return (dado1, dado2)

jogaDuasVezes' :: [(Dado, Dado)]
jogaDuasVezes' = jogaDado >>= \dado1 ->
                   magica dado1 >>= \dado2 ->
                    return (dado1, dado2)

pares :: [a] -> [b] -> [(a,b)]
pares xs ys = do x <- xs
                 y <- ys
                 return (x,y)

safeDiv :: (Show a, Eq a, Floating a) => a -> a -> Either String a
safeDiv a b
  | b == 0 = Left ("Divisao por zero: " ++ show a ++ "/" ++ show b)
  | otherwise = Right (a/b)

safeLog :: (Show a, Ord a, Eq a, Floating a) => a -> Either String a
safeLog x
  | x <= 0    = Left ("Log invalido: " ++ show x)
  | otherwise = Right (log x)

safeAdd :: Floating a => a -> a -> Either String a
safeAdd a b = Right (a+b)

safeExpr1, safeExpr2 :: Either String Double
safeExpr1 = do q <- safeDiv 10 (-2)
               l <- safeLog q
               safeAdd 2 q

safeExpr2 = do q <- safeDiv 10 0
               safeAdd 2 q

-- Efeitos Colaterais: Read Only

newtype Globais = Globais {
  y :: Int
}

data Reader e a = Reader (e -> a)

runReader :: Reader e a -> e -> a
runReader (Reader f) e = f e

ask :: Reader e e
ask = Reader id

instance Functor (Reader e) where
  -- fmap :: (a -> b) -> Reader e a -> Reader e b
  fmap f (Reader g) = Reader (f . g)

instance Applicative (Reader e) where
  -- pure :: a -> Reader e a
  pure x = Reader (\e -> x)

  -- <*> :: Reader e (a -> b) -> Reader e a -> Reader e b
  rFun <*> rV = Reader $ \e ->
    let f = runReader rFun e -- Passo 1
        v = runReader rV e in -- Passo 2
      f v

instance Monad (Reader e) where
  -- (>>=) :: Reader e a -> (a -> Reader e b) -> Reader e b
  rV >>= f = Reader $ \e ->
    let v = runReader rV e -- :: a
        r = f v in
      runReader r e

f_read0 :: Int -> Globais -> Int
f_read0 x gl = x + y gl

g_read0 :: Int -> Globais -> Int
g_read0 x gl = x * y gl

h_read0 :: Int -> Int -> Globais -> Bool
h_read0 a b gl = (vf + vg) /= 0
  where
    vf = f_read0 a gl -- Passando o contexto gl para f_read0
    vg = g_read0 b gl -- Passando o contexto gl para g_read0

f_read1 :: Int -> Reader Globais Int
f_read1 x = do
  g <- ask
  return $ x + y g

g_read1 :: Int -> Reader Globais Int
g_read1 x = do
  gl <- ask
  return $ x * y gl

h_read1 :: Int -> Int -> Reader Globais Bool
h_read1 a b = do
  vf <- f_read1 a -- O contexto é passado automaticamente!
  vg <- g_read1 b
  return $ (vf + vg) /= 0

askFor :: (e -> a) -> Reader e a
askFor f = fmap f ask

f_read2 :: Int -> Reader Globais Int
f_read2 x = do
  vy <- askFor y
  return $ x + vy

-- Efeitos Colaterais: Write Only
{-
isEven x = even x
not'   b = not b
isOdd  x = (not' . isEven) x
-}

isEven :: Integer -> (Bool, String)
isEven x = (even x, " isEven ")

not' :: Bool -> (Bool, String)
not'   b = (not b, " not' ")

isOdd :: Integer -> (Bool, String)
isOdd  x = let (b1, trace1) = isEven x
               (b2, trace2) = not' b1
           in (b2, trace1 ++ trace2)

data Writer w a = Writer a w

runWriter :: Writer w a -> (a, w)
runWriter (Writer a w) = (a, w)

tell :: w -> Writer w ()
tell s = Writer () s

instance (Monoid w) => Functor (Writer w) where
  -- fmap :: (a -> b) -> Writer w a -> Writer w b
  fmap f (Writer a w) = Writer (f a) w

instance (Monoid w) => Applicative (Writer w) where
  -- pure :: a -> Writer w a
  pure x = Writer x mempty

  -- (<*>) :: Writer w (a -> b) -> Writer w a -> Writer w b
  (Writer f m1) <*> (Writer a m2) = Writer (f a) (m1 <> m2)

instance (Monoid w) => Monad (Writer w) where
  -- (Writer w a) -> (a -> Writer w b) -> Writer w b
  (Writer a w) >>= k = let (b, w') = runWriter (k a)
                       in Writer b (w <> w')

isEvenW' :: Integer -> Writer String Bool
isEvenW' x = do
  tell " even "
  return (even x)

notW' :: Bool -> Writer String Bool
notW' x = do
  tell " not "
  return (not x)

-- (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)
isOddW' :: Integer -> Writer String Bool
isOddW' = (isEvenW' >=> notW')

-- Efeitos Colaterais: Read and Write

data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

f tree = Node (Node (Leaf 0) (Leaf 1)) (Leaf 2)

rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf _) n = (Leaf n, n + 1)
rlabel (Node l r) n = (Node l' r', n'')
  where
    (l', n')  = rlabel l n  -- altera o estado de n
    (r', n'') = rlabel r n' -- altera o estado de n'

data State s a = State (s -> (a, s))

runState :: State s a -> s -> (a, s)
runState (State f) s = f s

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State (\s -> let (a, s') = g s
                                  in (f a, s'))

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure x = State (\s -> (x, s))

  -- (<*>) :: State s (a - b) -> State s a -> State s b
  sab <*> sa = State (\s -> let (f, s1) = runState sab s
                                (a, s2) = runState sa s1
                            in (f a, s2))

instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  sa >>= f = State (\s -> let (a, s1) = runState sa s
                              sb      = f a
                          in runState sb s1)

alabel :: Tree a -> State Int (Tree Int)
alabel (Leaf _) = pure Leaf <*> sInc
alabel (Node l r) = pure Node <*> alabel l <*> alabel r

mlabel :: Tree a -> State Int (Tree Int)
mlabel (Leaf _) = do n <- sInc
                     return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return $ Node l' r'

sInc :: State Int Int
sInc = State (\n -> (n, n+1))


