module Main where

import           Control.Concurrent ( threadDelay )
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import           Text.Parsec

import           Board
import           Laws ( isOver )
import           Board.InitialPosition
import           Display ( printBoard )
import           Evaluation ( mateValue )
import qualified Format.PGN.Import as PGN
import           Format.PGN.Export (toPGN)
import           Format.FEN ( toFEN )
import           Format.FEN.Import ( fromFEN )

import           Bot.Human
import           Bot.Random
import           Bot.Greedy
import           Bot.Minimax
import           Bot.AlphaBeta

play :: (Board -> IO Board) -> (Board -> IO Board) -> Board -> IO Board
play whitePlayer blackPlayer board = do
  printBoard board

  let bot = if nextMove board == White then whitePlayer else blackPlayer

  newBoard <- bot board

  print $ head $ history newBoard
  putStrLn $ toFEN newBoard

  --threadDelay 500000

  case isOver newBoard of
    Just r  -> return $ newBoard { result = Just r }
    Nothing -> play whitePlayer blackPlayer newBoard

perfTest :: IO ()
perfTest = do
  let whiteLadderMatesIn3 = fromFEN "8/K7/5k2/8/8/8/3R4/4R3 w - - 0 1"
  let result = Bot.AlphaBeta.maxValue whiteLadderMatesIn3 mateValue (-mateValue) 5
  print result

main :: IO ()
main = do
  -- perfTest

  --let whitePlayer = Bot.Human.makeMove
  --let whitePlayer = Bot.Random.makeMove
  let whitePlayer = Bot.Minimax.makeMove
  --let whitePlayer = Bot.AlphaBeta.makeMove
  let blackPlayer = Bot.Greedy.makeMove

  board <- play whitePlayer blackPlayer initialBoard

  printBoard board
  print $ reverse $ history board
  putStrLn $ toPGN board
  putStrLn $ toFEN board
  print (fromJust $ result board)
