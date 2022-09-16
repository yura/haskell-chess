module Evaluation where

import Board
import Laws

type Value = Int

mateValue :: Int
mateValue = minBound + 1

evaluatePosition :: Board -> Value 
evaluatePosition board@Board{..}
  | isMate board = mateValue
  | isDraw board = 0 
  | otherwise    = foldl calculateCost 0 squares

  where
    calculateCost :: Int -> Maybe Piece -> Int
    calculateCost result Nothing = result
    calculateCost result (Just (Piece pt c)) = result + cost pt * if c == nextMove then 1 else (-1)

cost :: PieceType -> Value 
cost Pawn    = 1
cost Knight  = 3
cost Bishop  = 3
cost Rook    = 5
cost Queen   = 9
cost _       = 0
