module Evaluation where

import qualified Data.Map as M
import Board
import Laws

type Value = Int ---Float

mateValue :: Value
mateValue = -999999

discount :: Value
discount = 1

evaluatePosition :: Board -> Value 
evaluatePosition board@Board{..}
  | isMate board = mateValue
  | isDraw board = 0 
  | otherwise    = sum $ map (\(_, Piece pt c) -> cost pt * if c == nextMove then 1 else (-1)) $ M.toList squares

cost :: PieceType -> Value 
cost Pawn    = 1000
cost Knight  = 3000
cost Bishop  = 3000
cost Rook    = 5000
cost Queen   = 9000
cost _       = 0
