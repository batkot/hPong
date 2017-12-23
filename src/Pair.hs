-- Probably already exists, implemented as n00b training
module Pair 
    ( Pair (..)
    , unitX
    , unitY
    , add
    , mult
    , minus
    ) where

data Pair a = Pair 
    { x :: a 
    , y :: a }
    deriving (Show, Eq)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure a = Pair a a
    (Pair f g) <*> (Pair x y) = Pair (f x) (g y)

liftPair :: (a -> a -> a) -> Pair a -> Pair a -> Pair a
liftPair f x y = f <$> x <*> y

add :: (Num a) => Pair a -> Pair a -> Pair a
add = liftPair (+)

mult :: (Num a) => Pair a -> Pair a -> Pair a
mult = liftPair (*)

unitX :: (Num a) => Pair a
unitX = Pair 1 0

unitY :: (Num a) => Pair a
unitY = Pair 0 1

minus :: (Num a) => Pair a -> Pair a
minus = fmap (*(-1))
