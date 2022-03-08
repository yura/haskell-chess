module Evaluation where

import qualified Data.Map as M
import Board
import Laws

type Value = Int

evaluatePosition :: Board -> Value 
evaluatePosition board@Board{..}
  | isMate board = if nextMove == White then minBound else maxBound 
  | isDraw board = 0 
  | otherwise    = sum $ map (\(_, Piece pt c) -> cost pt * if c == White then 1 else (-1)) $ M.toList squares
-- | otherwise    = sum $ map (\(_, Piece pt c) -> cost pt * if c == nextMove then 1 else (-1)) $ M.toList squares

cost :: PieceType -> Value 
cost Pawn    = 1
cost Knight  = 3
cost Bishop  = 3
cost Rook    = 5
cost Queen   = 9
cost _       = 0