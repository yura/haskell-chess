module Laws where

import           Data.List (nub)
import qualified Data.Map as Map
import           Board
import qualified Laws.Pawn   as P
import qualified Laws.Knight as K
import qualified Laws.Bishop as B
import           Laws.Util

leftDirection :: Square -> [Square]
leftDirection (col, row) = map (\c -> (c, row)) [pred col, pred (pred col)..head cols]

downDirection :: Square -> [Square]
downDirection (col, row) = map (\r -> (col, r)) [pred row, pred (pred row)..head rows]

rightDirection :: Square -> [Square]
rightDirection (col, row) = map (\c -> (c, row)) [succ col..last cols]

upDirection :: Square -> [Square]
upDirection (col, row) = map (\r -> (col, r)) [succ row..last rows]

rookMovesGrouped :: Square -> [[Square]]
rookMovesGrouped squareName = dropEmptyLists $ leftDirection squareName : downDirection squareName : rightDirection squareName : upDirection squareName : []

rookMoves :: Square -> [Square]
rookMoves = concat . rookMovesGrouped

queenMoves :: Square -> [Square]
queenMoves squareName = B.bishopMoves squareName ++ rookMoves squareName

kingMoves :: Square -> [Square]
kingMoves squareName = map (\group -> head group) (B.bishopMovesGrouped squareName ++ rookMovesGrouped squareName)

possibleMoves :: Color -> Board -> [Move]
possibleMoves color board = concatMap (\s -> P.pawnPossibleMoves color s board) $ pawnSquares color board

captureThreatSquares :: Square -> Piece -> Board -> [Square]
captureThreatSquares square (Piece Pawn color)   board = P.captureThreatSquares color square board
captureThreatSquares square (Piece Knight color) board = K.captureThreatSquares color square board
captureThreatSquares square (Piece Bishop color) board = B.captureThreatSquares color square board
captureThreatSquares _ _ _ = []

allCaptureThreatSquares :: Color -> Board -> [Square]
allCaptureThreatSquares color board@(Board squares enPassantTarget)
  = nub $ concatMap (\(square, piece) -> captureThreatSquares square piece board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == color) squares

isCheck :: Color -> Board -> Bool
isCheck color board = kingAt color board `elem` allCaptureThreatSquares (opponent color) board

isMate :: Color -> Board -> Bool
isMate color board = False
