module Laws where

import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (fromJust, isJust)

import           Board
import qualified Laws.Pawn   as P
import qualified Laws.Knight as N
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import qualified Laws.Queen  as Q
import qualified Laws.King   as K
import           Laws.Util

possibleMoves :: Color -> Board -> [Move]
possibleMoves color board = concatMap (\s -> P.pawnPossibleMoves color s board) $ pawnSquares color board

-- Фигуры соперника, которые находятся под угрозой взятия.
captureThreatSquares :: Piece -> Square -> Board -> [Square]
captureThreatSquares (Piece Pawn color)   square board = P.captureThreatSquares color square board
captureThreatSquares (Piece Knight color) square board = N.captureThreatSquares color square board
captureThreatSquares (Piece Bishop color) square board = B.captureThreatSquares color square board
captureThreatSquares (Piece Rook color)   square board = R.captureThreatSquares color square board
captureThreatSquares (Piece Queen color)  square board = Q.captureThreatSquares color square board
-- Так как короли не могут приближаться друг к другу, то
-- ситуация, когда один король шахует другого невозможна. 
-- Иначе будет бесконечный цикл.
captureThreatSquares (Piece King color)   square _     = []

allCaptureThreatSquares :: Color -> Board -> [Square]
allCaptureThreatSquares color board@(Board squares enPassantTarget)
  = nub $ concatMap (\(square, piece) -> captureThreatSquares piece square board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == color) squares

-- Поля, которые находятся под угрозой шаха. Если король
-- соперника находится на одном из этих полей, то ему шах
-- и ходить на эти поля королю нельзя.
checkThreatSquares :: Piece -> Square -> Board -> [Square]
checkThreatSquares (Piece Pawn color)   square board = P.underAttackSquares board color square
checkThreatSquares (Piece Knight color) square _     = N.underAttackSquares square
checkThreatSquares (Piece Bishop color) square board = B.underAttackSquares board color square
checkThreatSquares (Piece Rook color)   square board = R.captureThreatSquares color square board
checkThreatSquares (Piece Queen color)  square board = Q.captureThreatSquares color square board
-- Так как короли не могут приближаться друг к другу, то
-- ситуация, когда один король шахует другого невозможна. 
-- Иначе будет бесконечный цикл.
checkThreatSquares (Piece King color)   square _     = []

allCheckThreatSquares :: Color -> Board -> [Square]
allCheckThreatSquares color board@(Board squares enPassantTarget)
  = nub $ concatMap (\(square, piece) -> checkThreatSquares piece square board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == opponent color) squares
  -- = nub $ concatMap (\(square, piece) -> checkThreatSquares piece square board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == color) squares

-- Допустимые ходы в соответствии с правилами:
-- * нельзя ходить под шах
kingValidMoveSquares :: Color -> Square -> Board -> [Square]
kingValidMoveSquares color square board = filter (\s -> s `notElem` allCheckThreatSquares color board) $ K.moveSquares square

kingCaptureThreatSquares :: Color -> Square -> Board -> [Square]
kingCaptureThreatSquares color square board
  = map fromJust
  $ filter isJust
  $ map (findOrStop color board)
  $ map (:[])
  $ kingValidMoveSquares color square board

isCheck :: Color -> Board -> Bool
isCheck color board = kingAt color board `elem` allCheckThreatSquares (opponent color) board

isMate :: Color -> Board -> Bool
isMate color board = False
