module Pair 
    ( Pair (..)
    , add
    ) where

data Pair a = Pair a a deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

add :: (Num a) => Pair a -> Pair a -> Pair a
add x y = (+) <$> x <*> y
