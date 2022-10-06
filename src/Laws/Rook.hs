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
w square@(Square s) =  map (\i -> Square $ s - i) $ take (squareFile square) $ [1, 2..]

s :: Square -> [Square]
s square@(Square s) =  map (\i -> Square $ s - i) $ take (squareRank square) $ [8, 16..]

e :: Square -> [Square]
e square@(Square s) =  map (\i -> Square $ s + i) $ take (lastFile - squareFile square) $ [1, 2..]

n :: Square -> [Square]
n square@(Square s) =  map (\i -> Square $ s + i) $ take (lastRank - squareRank square) $ [8, 16..]

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
