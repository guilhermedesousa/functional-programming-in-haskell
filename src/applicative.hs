{-

pure   :: a -> f a
aplica :: f (a -> b) -> f a -> f b

fmap0 = pure
fmap1 g x = aplica (pure g) x
fmap2 g x y = aplica (aplica (pure g) x) y

-}

maybeDiv x 0 = Nothing
maybeDiv x y = Just (x `div` y)

