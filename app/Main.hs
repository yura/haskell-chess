module Main where

import Control.Concurrent (threadDelay)
import Data.Char
import Data.Text as T

import Board
import Board.InitialPosition
import Display

clearScreen :: IO ()
clearScreen = do
  putStrLn $ chr 27 : "[2J"
  putStrLn $ chr 27 : "[;H"

main :: IO ()
main = do
  clearScreen
  putStrLn "Samolet Chess"

  threadDelay 1000000
  clearScreen
  putStrLn $ T.unpack $ exportToDisplay emptyBoard True

  threadDelay 1000000
  clearScreen
  putStrLn $ T.unpack $ exportToDisplay initialBoard True
