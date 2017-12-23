module Graphics
    where

import System.Console.ANSI 
import System.IO

import Data.List ((\\))

import Pair
import Pong

draw :: String -> Pos ->  IO ()
draw x (Pair c r) = do
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
    
printBoard :: Table -> IO ()
printBoard (Table w h) = do
    clearScreen
    setCursorPosition 0 0 
    putStr line
    setCursorPosition (h + 1) 0
    putStr line
    where
        line = " " ++ replicate w '-' ++ " "

printState :: State -> IO()
printState (State ball board p1 p2) = 
    printBoard board >>
    printBall ball >>
    printRocket p1 >>
    printRocket p2
    where
        printBall (Ball p _) = draw "@" p
        printRocket (Rocket r) = mapM_ (draw "|") r
