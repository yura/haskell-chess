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

possibleMoves :: Color -> Board -> [Move]
possibleMoves color board = pawnMoves ++ knightMoves ++ bishopMoves ++ rookMoves ++ queenMoves ++ kingMoves
  where
    pawnMoves   = concatMap (pawnValidMoves board color)   $ pawnSquares color board
    knightMoves = concatMap (knightValidMoves board color) $ knightSquares color board
    bishopMoves = concatMap (bishopValidMoves board color) $ bishopSquares color board
    rookMoves   = concatMap (rookValidMoves board color)   $ rookSquares color board
    queenMoves  = concatMap (queenValidMoves board color)  $ queenSquares color board
    kingMoves   = concatMap (kingValidMoves board color)   $ kingSquares color board

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
allCaptureThreatSquares color board@Board{..}
  = nub $ concatMap (\(square, piece) -> captureThreatSquares piece square board) $ Map.toList $ Map.filter (\(Piece _ c) -> c == color) squares

-- Поля, которые находятся под угрозой шаха. Если король
-- соперника находится на одном из этих полей, то ему шах
-- и ходить на эти поля королю нельзя.
checkThreatSquares :: Piece -> Square -> Board -> [Square]
checkThreatSquares (Piece Pawn color)   square board = P.underAttackSquares board color square
checkThreatSquares (Piece Knight color) square _     = N.underAttackSquares square
checkThreatSquares (Piece Bishop color) square board = B.underAttackSquares board color square
checkThreatSquares (Piece Rook color)   square board = R.underAttackSquares board color square
checkThreatSquares (Piece Queen color)  square board = Q.underAttackSquares board color square
-- Так как короли не могут приближаться друг к другу, то
-- ситуация, когда один король шахует другого невозможна. 
-- Иначе будет бесконечный цикл.
checkThreatSquares (Piece King color)   square _     = K.moveSquares square

allCheckThreatSquares :: Color -> Board -> [Square]
allCheckThreatSquares color board@(Board{..})
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

kingValidMoves :: Board -> Color -> Square -> [Move]
kingValidMoves board color square = filter (not . isCheck color . move board) $  K.possibleMoves board color square 

-- * нельзя ходить под шах
kingValidMoveSquares :: Color -> Square -> Board -> [Square]
kingValidMoveSquares color square board = filter (not . isCheck color . moveKing) $ K.moveSquares square
  where
    moveKing targetSquare = movePiece square targetSquare (Piece King color) board

kingCaptureThreatSquares :: Color -> Square -> Board -> [Square]
kingCaptureThreatSquares color square board
  = mapMaybe (findOrStop color board . (:[])) (kingValidMoveSquares color square board)

-- Ничья?
-- https://en.wikipedia.org/wiki/Draw_(chess)
-- https://ru.wikipedia.org/wiki/%D0%9D%D0%B8%D1%87%D1%8C%D1%8F_(%D1%88%D0%B0%D1%85%D0%BC%D0%B0%D1%82%D1%8B)
isDraw :: Color -> Board -> Bool 
isDraw color board | white == [kingWhite] && black == [kingBlack] = True
                   | white == [bishopWhite, kingWhite] && black == [kingBlack] = True
                   | white == [kingWhite] && black == [bishopBlack, kingBlack] = True
                   | white == [knightWhite, kingWhite] && black == [kingBlack] = True
                   | white == [kingWhite] && black == [knightBlack, kingBlack] = True
                   | otherwise = False
  where
    white = pieces White board
    black = pieces Black board

-- Шах?
isCheck :: Color -> Board -> Bool
isCheck color board = kingAt color board `elem` allCheckThreatSquares (opponent color) board

-- Пат?
isStalemate :: Color -> Board -> Bool 
isStalemate color board = not (isCheck color board) && null (possibleMoves color board) 

-- Мат?
isMate :: Color -> Board -> Bool
isMate color board = isCheck color board && null (possibleMoves color board)

result :: Color -> Board -> Maybe Result 
result color board | isMate color board      = Just $ if color == White then BlackWon else WhiteWon
                   | isStalemate color board = Just Stalemate
                   | isDraw color board      = Just Draw
                   | otherwise               = Nothing

