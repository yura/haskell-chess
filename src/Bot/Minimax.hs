module Bot.Minimax where

import Debug.Trace

import Data.List (sortOn)
import Data.Maybe (isJust)
import Board
import Laws
import Evaluation (Value, discount, evaluatePosition)

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  let (_, m) = maxValue board 3
  return $ move board m

maxValue :: Board -> Int -> (Value, Move)
maxValue board depth | isJust (isOver board) || depth == 0 = (evaluatePosition board - depth, head $ history board)
                     | otherwise = (v, m)
  where
    (v, m) = last results
    results = sortOn fst
      $ map (\m -> ((-1) * fst (maxValue (move board m) (depth - 1)), m))
      $ possibleMoves board
