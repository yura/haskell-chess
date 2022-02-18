module Laws where

import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)

import           Board
import qualified Laws.Pawn   as P
import qualified Laws.Knight as N
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import qualified Laws.Queen  as Q
import qualified Laws.King   as K
import           Laws.Util
import Laws.Pawn (pawnPossibleMoves)

possibleMoves :: Color -> Board -> [Move]
possibleMoves color board = pawnMoves ++ knightMoves ++ bishopMoves ++ rookMoves ++ queenMoves ++ kingMoves
  where
    pawnMoves   = concatMap (pawnValidMoves board color)   $ pawnSquares color board
    knightMoves = concatMap (knightValidMoves board color) $ knightSquares color board
    bishopMoves = concatMap (bishopValidMoves board color) $ bishopSquares color board
    rookMoves   = concatMap (rookValidMoves board color)   $ rookSquares color board
    queenMoves  = concatMap (queenValidMoves board color)  $ queenSquares color board
    kingMoves = []

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
captureThreatSquares (Piece King color)   square board = kingCaptureThreatSquares color square board -- K.captureThreatSquares color square board

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
checkThreatSquares (Piece King color)   square _     = K.moveSquares square

allCheckThreatSquares :: Color -> Board -> [Square]
allCheckThreatSquares color board@(Board squares enPassantTarget)
  = nub $ concatMap (\(square, piece) -> checkThreatSquares piece square board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == color) squares

-- Допустимые ходы в соответствии с правилами:
pawnValidMoves :: Board -> Color -> Square -> [Move]
pawnValidMoves board color square = filter (not . isCheck color . move board) $ P.pawnPossibleMoves color square board

knightValidMoves :: Board -> Color -> Square -> [Move]
knightValidMoves board color square = filter (not . isCheck color . move board) $ N.possibleMoves board color square 

bishopValidMoves :: Board -> Color -> Square -> [Move]
bishopValidMoves board color square = filter (not . isCheck color . move board) $ B.possibleMoves board color square 

rookValidMoves :: Board -> Color -> Square -> [Move]
rookValidMoves board color square = filter (not . isCheck color . move board) $ R.possibleMoves board color square 

queenValidMoves :: Board -> Color -> Square -> [Move]
queenValidMoves board color square = filter (not . isCheck color . move board) $ Q.possibleMoves board color square 

-- * нельзя ходить под шах
kingValidMoveSquares :: Color -> Square -> Board -> [Square]
kingValidMoveSquares color square board = filter (not . isCheck color . moveKing) $ K.moveSquares square
  where
    moveKing targetSquare = movePiece square targetSquare (Piece King color) board

kingCaptureThreatSquares :: Color -> Square -> Board -> [Square]
kingCaptureThreatSquares color square board
  = mapMaybe (findOrStop color board . (:[])) (kingValidMoveSquares color square board)

isCheck :: Color -> Board -> Bool
isCheck color board = kingAt color board `elem` allCheckThreatSquares (opponent color) board

isMate :: Color -> Board -> Bool
isMate color board = False