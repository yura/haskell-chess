{-# LANGUAGE TupleSections #-}

module Laws.Rook where

import Data.Maybe (mapMaybe)
import Board
import Laws.Util

pieceType :: PieceType
pieceType = Rook

piece :: Color -> Piece
piece = Piece pieceType

w :: Square -> [Square]
w (col, row) = map (, row) [pred col, pred (pred col)..head cols]

s :: Square -> [Square]
s (col, row) = map (col, ) [pred row, pred (pred row)..head rows]

e :: Square -> [Square]
e (col, row) = map (, row) [succ col..last cols]

n :: Square -> [Square]
n (col, row) = map (col, ) [succ row..last rows]

rookMovesGrouped :: Square -> [[Square]]
rookMovesGrouped square = filter (not . null) $ map (\f -> f square) [w, s, e, n]

rookMoves :: Square -> [Square]
rookMoves = concat . rookMovesGrouped

moveSquares :: Color -> Square -> Board -> [Square]
moveSquares color square board =  filter (`notElem` captureThreatSquares color square board) $ underAttackSquares board color square 

underAttackSquares :: Board -> Color -> Square -> [Square]
underAttackSquares board color square = concatMap (filterAllEmptyOrFirstOpposite board color) $ rookMovesGrouped square

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board = mapMaybe (findOrStop color board) (rookMovesGrouped square)

possibleMoves :: Board -> Color -> Square -> [Move]
possibleMoves board color square = captures ++ moves
  where
    captures = map (Capture (piece color) square) $ captureThreatSquares color square board
    moves    = map (Move (piece color) square)    $ moveSquares color square board
