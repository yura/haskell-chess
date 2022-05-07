module Bot.AlphaBeta where

import Control.Parallel.Strategies
import Data.Bifunctor ( second )
import Data.Function ( on )
import Data.List ( find, maximumBy, minimumBy, sortOn )
import Data.Maybe ( isJust )
import Board
import Laws
import Bot.Random
import Evaluation

import Debug.Trace
import Display (moveToDisplay)

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  let (v1, m1) = maxValue board mateValue (-mateValue) 1
  let (v3, m3) = maxValue board mateValue (-mateValue) 3

  return $ if v1 == abs mateValue then move board m1 else move board m3

maxValue :: Board -> Int -> Int -> Int -> (Value, Move)
maxValue board alpha beta depth | isJust (isOver board) || depth == 0 = (evaluatePosition board, head $ history board)
                                | otherwise = maxIteration (possibleMoves board) mateValue (QueensideCastling White) alpha beta
  where
    maxIteration (m:ms) bestValue bestMove alpha beta = if bestValue' >= beta then (bestValue', bestMove') else maxIteration ms bestValue' bestMove' currentAlpha beta
      where
        (minVal, minMove) = minValue (move board m) alpha beta (pred depth)
        (bestValue', bestMove', currentAlpha) = if minVal > bestValue then (minVal, minMove, max alpha minVal) else (bestValue, bestMove, alpha)

minValue :: Board -> Int -> Int -> Int -> (Value, Move)
minValue board alpha beta depth | isJust (isOver board) || depth == 0 = (evaluatePosition board, head $ history board)
                                | otherwise = result
  where
    -- result = last $ traceShow ((if null (history board) then "" else show (moveToDisplay (head (history board)))) ++ " min: " ++ show (map (second moveToDisplay) results)) results
    result = last results
    results = sortOn fst
      $ map (\m -> ((-1) * fst (maxValue (move board m) alpha beta (pred depth)), m))
      $ possibleMoves board
