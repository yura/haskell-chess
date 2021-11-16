module Main where

import Control.Concurrent (threadDelay)
import Data.Char

clearScreen :: IO ()
clearScreen = do
  putStrLn $ chr 27 : "[2J"
  putStrLn $ chr 27 : "[;H"

main :: IO ()
main = do
  clearScreen
  putStrLn "Hello chess"
  threadDelay 1000000
  clearScreen
  putStrLn "Hey"
