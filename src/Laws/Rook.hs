module Laws.Rook where

import Data.Maybe (fromJust, isJust)
import Board
import Laws.Util

w :: Square -> [Square]
w (col, row) = map (\c -> (c, row)) [pred col, pred (pred col)..head cols]

s :: Square -> [Square]
s (col, row) = map (\r -> (col, r)) [pred row, pred (pred row)..head rows]

e :: Square -> [Square]
e (col, row) = map (\c -> (c, row)) [succ col..last cols]

n :: Square -> [Square]
n (col, row) = map (\r -> (col, r)) [succ row..last rows]

rookMovesGrouped :: Square -> [[Square]]
rookMovesGrouped square = filter (not . null) $ map (\f -> f square) [w, s, e, n]

rookMoves :: Square -> [Square]
rookMoves = concat . rookMovesGrouped

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = map fromJust
  $ filter isJust
  $ map (findOrStop color board)
  $ rookMovesGrouped square
