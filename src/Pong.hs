module Pong
    ( Pos 
    , Table (..)
    , Ball (..)
    , Rocket (..)
    , State (..)
    , GameCommand (..)
    , RocketCommand (..)
    , initBall 
    , updateState
    , gameLoop
    , mkRocket
    ) where

import Pair

type Pos = Pair Int
data Table = Table Int Int deriving Show
data Ball = Ball Pos Pos deriving Show
newtype Rocket = Rocket [Pos] deriving Show

mkRocket :: Int -> Int -> Int -> Rocket
mkRocket r s l = Rocket $ fmap (Pair r) [s..(s+l)]

data State = State 
    { ball :: Ball
    , table :: Table
    , playerOne :: Rocket
    , playerTwo :: Rocket } deriving Show

data RocketCommand = Up
                   | Down
                   deriving (Show, Eq)

data GameCommand = PlayerOne RocketCommand
                 | PlayerTwo RocketCommand
                 | Tick 
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

updateState :: State -> GameCommand -> State
updateState s@(State ball box (Rocket p1) (Rocket p2)) Tick = s { ball = updateBall ball box (p1 ++ p2) }
updateState s@(State _ box p _) (PlayerOne cmd) = s { playerOne = updateRocket p box cmd }
updateState s@(State _ box _ p) (PlayerTwo cmd) = s { playerTwo = updateRocket p box cmd }

gameLoop :: Monad m => (State -> State -> m()) -> m [GameCommand] -> State -> m State
gameLoop render cmds state = 
    fmap (calcState . (:) Tick) cmds
    >>= (\newState -> 
            render state newState 
            >> gameLoop render cmds newState)
    where
        calcState = foldl updateState state 
