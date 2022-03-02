module Evaluation where

import qualified Data.Map as M
import Board
import Laws

evaluatePosition :: Board -> Int 
evaluatePosition board@Board{..}
  | isMate board = minBound 
  | isDraw board = 0 
  | otherwise    = sum $ map (\(_, Piece pt c) -> cost pt * if c == nextMove then 1 else (-1)) $ M.toList squares

cost :: PieceType -> Int 
cost Pawn    = 1
cost Knight  = 3
cost Bishop  = 3
cost Rook    = 5
cost Queen   = 9
cost _       = 0