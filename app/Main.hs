module Main where

import System.Timeout
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import System.Console.ANSI 
import System.IO
import Data.Maybe (catMaybes)
import Data.List (nub, (\\))

import Lib

second :: Int
second = 1000000;

fpsToInterval :: Int -> Int
fpsToInterval x = second `div` x

data Pos = Pos Int Int deriving (Eq)
data BoxSize = BoxSize Int Int
data Ball = Ball Pos Pos
newtype Rocket = Rocket [Pos]
data State = State 
    { ball :: Ball
    , size :: BoxSize 
    , playerOne :: Rocket
    , playerTwo :: Rocket }

data RocketCommand = Up
                   | Down
                   deriving (Show, Eq)

data GameCommand = PlayerOne RocketCommand
                 | PlayerTwo RocketCommand
                 | Tick 
                 deriving (Show, Eq)

upPos :: Pos
upPos = Pos 0 (-1) 

downPos :: Pos
downPos = Pos 0 1

addPos :: Pos -> Pos -> Pos
addPos (Pos c r) (Pos c2 r2) = Pos (c + c2) (r + r2)

playerOneRocket :: Rocket
playerOneRocket = Rocket [Pos 0 13, Pos 0 14, Pos 0 15, Pos 0 16]

playerTwoRocket :: Rocket
playerTwoRocket = Rocket [Pos 100 13, Pos 100 14, Pos 100 15, Pos 100 16]

boardSize :: BoxSize 
boardSize = BoxSize 100 30

initBall :: Ball
initBall = Ball (Pos 50 15) (Pos 1 1)

emptyState :: State
emptyState = State initBall boardSize playerOneRocket playerTwoRocket

speed :: Int
speed = fpsToInterval 20

printBoard :: BoxSize -> IO ()
printBoard (BoxSize w h) = do
    clearScreen
    setCursorPosition 0 0 
    putStr line
    setCursorPosition (h + 1) 0
    putStr line
    where
        line = " " ++ replicate w '-' ++ " "

draw :: String -> Pos ->  IO ()
draw x (Pos c r) = do
    setCursorPosition r c
    putStr x

move :: Pos -> Pos -> String -> IO()
move old new g
    | old == new    = return ()
    | otherwise     = draw " " old >> draw g new 

renderBallDiff :: Ball -> Ball -> IO ()
renderBallDiff (Ball oldPos _) (Ball newPos _) = move oldPos newPos "@"

renderStateDiff :: State -> State -> IO()
renderStateDiff (State oldBall _ oldP1 oldP2) (State newBall _ newP1 newP2) =
    renderBallDiff oldBall newBall 
    >> renderRocketDiff oldP1 newP1 
    >> renderRocketDiff oldP2 newP2 
    >> hFlush stdout

renderRocketDiff :: Rocket -> Rocket -> IO()
renderRocketDiff (Rocket old) (Rocket new) = do
    mapM_ (draw " ") toHide
    mapM_ (draw "|") toRender
    where
        toHide = old \\ new
        toRender = new \\ old
    

main :: IO ()
main = do
    hSetEcho stdin False 
    hSetBuffering stdin NoBuffering 
    hideCursor
    setTitle "hPong"
    printState emptyState
    void $ gameLoop (renderStateDiff emptyState) readCommands emptyState

printState :: State -> IO()
printState (State ball board p1 p2) = 
    printBoard board >>
    printBall ball >>
    printRocket p1 >>
    printRocket p2
    where
        printBall (Ball p _) = draw "@" p
        printRocket (Rocket r) = mapM_ (draw "|") r
    

gameLoop :: (State -> IO()) -> IO [GameCommand] -> State -> IO State
gameLoop render cmds state = 
    fmap (calcState . (:) Tick) cmds
    >>= (\newState -> 
            render newState 
            >> threadDelay speed 
            >> gameLoop (renderStateDiff newState) cmds newState)
    where
        calcState = foldl updateState state 

updateBall :: Ball -> BoxSize -> [Pos] -> Ball
updateBall (Ball (Pos c r) (Pos vc vr)) (BoxSize w h) rockets
    | newR == 0 || newR == h + 1 = Ball (Pos c r) (Pos vc (-vr))
    | (newC == 0 || newC == w) && newPos `elem` rockets = Ball (Pos c r) (Pos (-vc) vr)
    | newC == 0 || newC == w = initBall
    | otherwise = Ball (Pos newC newR) (Pos vc vr)
    where
        newR = r + vr
        newC = c + vc 
        newPos = Pos newC newR

updateRocket :: Rocket -> BoxSize -> RocketCommand -> Rocket
updateRocket r@(Rocket (Pos _ 0:_)) _ Up = r
updateRocket r@(Rocket rp) _ Up = Rocket $ fmap (addPos upPos) rp
updateRocket r@(Rocket rp) (BoxSize _ h) Down 
    | maxRow rp == h = r
    | otherwise      = Rocket $ fmap (addPos downPos) rp
    where 
        extractRow (Pos _ r) = r
        maxRow = maximum . fmap extractRow

updateState :: State -> GameCommand -> State
updateState s@(State ball box (Rocket p1) (Rocket p2)) Tick = s { ball = updateBall ball box (p1 ++ p2) }
updateState s@(State _ box p _) (PlayerOne cmd) = s { playerOne = updateRocket p box cmd }
updateState s@(State _ box _ p) (PlayerTwo cmd) = s { playerTwo = updateRocket p box cmd }

readCommands :: IO [GameCommand]
readCommands = fmap parseCmds readStdin 

parseCmds :: String -> [GameCommand]
parseCmds = nub . catMaybes . fmap mapControls

readStdin :: IO String
readStdin = read' []
    where
        read' :: String -> IO String
        read' s = do 
            ready <- hReady stdin
            if ready then getChar >>= \n -> read' (n:s) else return s

mapControls :: Char -> Maybe GameCommand
mapControls 'w' = Just $ PlayerOne Up
mapControls 's' = Just $ PlayerOne Down
mapControls 'i' = Just $ PlayerTwo Up
mapControls 'k' = Just $ PlayerTwo Down
mapControls _ = Nothing
