module Pong
    ( Pos 
    , Table (..)
    , Ball (..)
    , Rocket (..)
    , PongState (..)
    , GameCommand (..)
    , RocketCommand (..)
    , initBall 
    , updateState
    , gameLoop
    , mkRocket
    , PongGameMonad(..)
    ) where

import Pair

type Pos = Pair Int
data Table = Table Int Int deriving Show
data Ball = Ball Pos Pos deriving Show
newtype Rocket = Rocket [Pos] deriving Show

mkRocket :: Int -> Int -> Int -> Rocket
mkRocket r s l = Rocket $ fmap (Pair r) [s..(s+l)]

data PongState = PongState
    { ball :: Ball
    , table :: Table
    , playerOne :: Rocket
    , playerTwo :: Rocket 
    } deriving Show

data RocketCommand 
    = Up
    | Down
    deriving (Show, Eq)

data GameCommand 
    = PlayerOne RocketCommand
    | PlayerTwo RocketCommand
    | Tick 
    | Quit
    deriving (Show, Eq)

initBall :: Ball
initBall = Ball (Pair 50 15) (Pair 1 1)

up :: Pos
up = minus down

down :: Pos
down = unitY

updateBall :: Ball -> Table -> [Pos] -> Ball
updateBall (Ball bp bv) (Table w h) rockets
    | newR == 0 || newR == h + 1 = Ball  bp (bounceH `mult` bv)
    | (newC == 0 || newC == w) && newPos `elem` rockets = Ball bp (bounceV `mult` bv)
    | newC == 0 || newC == w = initBall
    | otherwise = Ball newPos bv
    where
        newPos = bp `add` bv
        newC = x newPos
        newR = y newPos
        bounceH = unitX `add` minus unitY
        bounceV = minus unitX `add` unitY

updateRocket :: Rocket -> Table  -> RocketCommand -> Rocket
updateRocket r@(Rocket (Pair _ 0:_)) _ Up = r
updateRocket r@(Rocket rp) _ Up = Rocket $ fmap (add up) rp
updateRocket r@(Rocket rp) (Table _ h) Down 
    | maxRow rp == h = r
    | otherwise      = Rocket $ fmap (add down) rp
    where
        extractRow (Pair _ r) = r
        maxRow = maximum . fmap extractRow

updateState :: PongState -> GameCommand -> PongState
updateState s@(PongState ball box (Rocket p1) (Rocket p2)) Tick = s { ball = updateBall ball box (p1 ++ p2) }
updateState s@(PongState _ box p _) (PlayerOne cmd) = s { playerOne = updateRocket p box cmd }
updateState s@(PongState _ box _ p) (PlayerTwo cmd) = s { playerTwo = updateRocket p box cmd }

gameLoop :: PongGameMonad m => PongState -> m PongState
gameLoop state = 
    fmap (calcState . (:) Tick) getCommands
    >>= (\newState ->
        renderState state newState
        >> gameLoop newState)
  where
    calcState = foldl updateState state 

class Monad m => PongGameMonad m where
    renderState :: PongState -> PongState -> m ()
    getCommands :: m [GameCommand]
