module Main where

import System.Timeout
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Control.Monad
import System.Console.ANSI 
import System.IO
import Data.Maybe (catMaybes)
import Data.List (nub, (\\))

import Pair 
import Pong
import Graphics

second :: Int
second = 1000000;

fpsToInterval :: Int -> Int
fpsToInterval x = second `div` x

playerOneRocket :: Rocket
playerOneRocket = mkRocket 0 13 4

playerTwoRocket :: Rocket
playerTwoRocket = mkRocket 100 13 4

boardSize :: Table
boardSize = Table 100 30

emptyState :: State
emptyState = State initBall boardSize playerOneRocket playerTwoRocket

speed :: Int
speed = fpsToInterval 20

main :: IO ()
main = do
    hSetEcho stdin False 
    hSetBuffering stdin NoBuffering 
    hideCursor
    setTitle "hPong"
    printState emptyState
    void $ gameLoop renderStateDiff readCommands emptyState

-- INPUT
readCommands :: IO [GameCommand]
readCommands = threadDelay speed >> fmap parseCmds readStdin 

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
