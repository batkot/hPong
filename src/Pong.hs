module Pong
    ( ) where

import Pair

newtype Pos = Pos (Pair Int)


makePos :: Int -> Int -> Pos
makePos x y = Pos $ Pair x y
