module Bot.Minimax where

import Debug.Trace

import Data.List (sortOn)
import Data.Maybe (isJust)
import Board
import Laws
-- import Bot.Random
import Evaluation (Value, discount, evaluatePosition)

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  let (v1, m1) = maxValue board 1
  let (v3, m3) = maxValue board 3

  --return $ if v1 == abs mateValue then move board m1 else move board m3
  return $ move board m3

maxValue :: Board -> Int -> (Value, Move)
maxValue board depth | isJust (isOver board) || depth == 0 = (evaluatePosition board, head $ history board)
                     | otherwise = (v - discount, m)
  where
    (v, m) = last results
    results = sortOn fst
      $ map (\m -> ((-1) * fst (minValue (move board m) (pred depth)) - discount, m))
      $ possibleMoves board

minValue :: Board -> Int -> (Value, Move)
minValue board depth | isJust (isOver board) || depth == 0 = (evaluatePosition board, head $ history board)
                     | otherwise = (v - discount, m)
  where
    (v, m) = last results
    results = sortOn fst
      $ map (\m -> ((-1) * fst (maxValue (move board m) (pred depth)), m))
      $ possibleMoves board
