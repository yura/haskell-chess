module Bot.Minimax where

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
  let (v1, m1) = maxValue board 1
  let (v3, m3) = maxValue board 3

  return $ if v1 == abs mateValue then move board m1 else move board m3

maxValue :: Board -> Int -> (Value, Move)
maxValue board depth | isJust (isOver board) || depth == 0 = (evaluatePosition board, head $ history board)
                     | otherwise = result
  where
    -- result = last $ traceShow ((if null (history board) then "" else show (moveToDisplay (head (history board)))) ++ " max: " ++ show (map (second moveToDisplay) results)) results
    result = last results
    results = sortOn fst
      $ map (\m -> ((-1) * fst (minValue (move board m) (pred depth)), m))
      $ possibleMoves board

minValue :: Board -> Int -> (Value, Move)
minValue board depth | isJust (isOver board) || depth == 0 = (evaluatePosition board, head $ history board)
                     | otherwise = result
  where
    -- result = last $ traceShow ((if null (history board) then "" else show (moveToDisplay (head (history board)))) ++ " min: " ++ show (map (second moveToDisplay) results)) results
    result = last results
    results = sortOn fst
      $ map (\m -> ((-1) * fst (maxValue (move board m) (pred depth)), m))
      $ possibleMoves board
