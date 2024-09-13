import Data.Semigroup
import Data.Foldable
import Data.Monoid
import Prelude hiding (sum)
import Data.Foldable (foldl')

{-

class Semigroup a where
    (<>) :: a -> a -> a

instance Semigroup [a] where
    (<>) = (++)

instance Semigroup a => Semigroup (Maybe a) where
    Nothing <> mb        = mb
    ma <> Nothing        = ma
    (Just x) <> (Just y) = Just (x <> y)

instance Num a => Semigroup (Sum a) where
    (Sum x) <> (Sum y) = Sum (x+y)

instance Num a => Semigroup (Product a) where
    (Product x) <> (Product y) = Product (x*y)

newtype Sum a = Sum { getSum :: a }

newtype Any = Any { getAny :: Bool }
newtype All = All { getAll :: Bool }

instance Semigroup Any where
    Any False <> Any False = Any False
    _ <> _                 = Any True

instance Semigroup All where
    All True <> All True = All True
    _ <> _               = All False

instance Semigroup (Last a) where
    _ <> Last x = Last x

instance Semigroup (First a) where
    First x <> _ = First x

instance Ord a => Semigroup (Min a) where
    Min x <> Min y = Min (min x y)

instance Ord a => Semigroup (Max a) where
    Max x <> Max y = Max (max x y)

-}

-- Semigroup

newtype UmInteiro = MKInteiro Int

instance Semigroup UmInteiro where
    (MKInteiro x) <> (MKInteiro y) = MKInteiro $ x + y

-- Monoid

instance Monoid UmInteiro where
    mempty = MKInteiro 0

-- Foldable

fold :: Monoid a => [a] -> a
fold []     = mempty
fold (x:xs) = x `mappend` Main.fold xs

data Tree a = Leaf a | Node (Tree a) (Tree a)
    deriving (Show)

-- instance Foldable [a] where
--     fold = mconcat
--     foldMap f xs = mconcat $ map f xs

instance Foldable Tree where
    foldMap f (Leaf x)   = f x
    foldMap f (Node l r) = foldMap f l <> foldMap f r

data Fold i o = forall m . Monoid m => Fold (i -> m) (m -> o)

fold' :: Fold i o -> [i] -> o
fold' (Fold toMonoid summarize) is =
    summarize (foldl' (<>) mempty (map toMonoid is))
    -- summarize (foldMap toMonoid xs)

sum :: Num n => Fold n n
sum = Fold Sum getSum