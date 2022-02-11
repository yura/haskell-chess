module Laws where

import           Data.List (nub)
import qualified Data.Map as Map
import           Board
import qualified Laws.Pawn   as P
import qualified Laws.Knight as K
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import           Laws.Util

queenMoves :: Square -> [Square]
queenMoves squareName = B.bishopMoves squareName ++ R.rookMoves squareName

kingMoves :: Square -> [Square]
kingMoves squareName = map (\group -> head group) (B.bishopMovesGrouped squareName ++ R.rookMovesGrouped squareName)

possibleMoves :: Color -> Board -> [Move]
possibleMoves color board = concatMap (\s -> P.pawnPossibleMoves color s board) $ pawnSquares color board

captureThreatSquares :: Square -> Piece -> Board -> [Square]
captureThreatSquares square (Piece Pawn color)   board = P.captureThreatSquares color square board
captureThreatSquares square (Piece Knight color) board = K.captureThreatSquares color square board
captureThreatSquares square (Piece Bishop color) board = B.captureThreatSquares color square board
captureThreatSquares square (Piece Rook color)   board = R.captureThreatSquares color square board
captureThreatSquares _ _ _ = []

allCaptureThreatSquares :: Color -> Board -> [Square]
allCaptureThreatSquares color board@(Board squares enPassantTarget)
  = nub $ concatMap (\(square, piece) -> captureThreatSquares square piece board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == color) squares

isCheck :: Color -> Board -> Bool
isCheck color board = kingAt color board `elem` allCaptureThreatSquares (opponent color) board

isMate :: Color -> Board -> Bool
isMate color board = False
