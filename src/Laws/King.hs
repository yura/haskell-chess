module Laws.King where

import           Data.Maybe ( mapMaybe )
import           Board hiding (canCastleKingside, canCastleQueenside)
import qualified Board as B (canCastleKingside, canCastleQueenside)

import {-# SOURCE #-} qualified Laws as L
import qualified Laws.Queen as Q
import           Laws.Util

initialRow :: Color -> Int
initialRow White = 1
initialRow Black = 8

initialPosition :: Color -> Square
initialPosition White = ('e', 1)
initialPosition Black = ('e', 8)

pieceType :: PieceType
pieceType = King

piece :: Color -> Piece
piece = Piece pieceType

-- Геометрически возможные ходы (по пустой доске)
moveSquares :: Square -> [Square]
moveSquares square = map head $ Q.queenMovesGrouped square

underAttackSquares :: Board -> Color -> Square -> [Square]
underAttackSquares board color square = concatMap (filterAllEmptyOrFirstOpposite board color . (:[])) (moveSquares square)

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = mapMaybe (findOrStop color board . (:[])) (moveSquares square)

canCastleKingside :: Board -> Color -> Bool
canCastleKingside board color
  =  findPiece board ('e', row) == Just (Piece King color)
  && findPiece board ('h', row) == Just (Piece Rook color)
  -- король и ладья раньше не ходили
  && B.canCastleKingside color board
  -- нет фигур между королём и ладьёй
  && not (taken board ('f', row)) && not (taken board ('g', row))
  -- король не под шахом
  -- можно использовать isCheck:
  -- && not (isCheck color board)
  && ('e', row) `notElem` L.allCheckThreatSquares (opponent color) board
  -- пройдёт не через битое поле
  && ('f', row) `notElem` L.allCheckThreatSquares (opponent color) board  
  -- встанет не под шах
  && ('g', row) `notElem` L.allCheckThreatSquares (opponent color) board  
  where row = initialRow color

canCastleQueenside :: Board -> Color -> Bool
canCastleQueenside board color
  =  findPiece board ('e', row) == Just (Piece King color)
  && findPiece board ('a', row) == Just (Piece Rook color)
  -- король и ладья раньше не ходили
  && B.canCastleQueenside color board
  -- нет фигур между королём и ладьёй
  && not (taken board ('d', row)) && not (taken board ('c', row)) && not (taken board ('b', row))
  -- король не под шахом
  -- можно использовать isCheck:
  -- && not (isCheck color board)
  && ('e', row) `notElem` L.allCheckThreatSquares (opponent color) board
  -- пройдёт не через битое поле
  && ('d', row) `notElem` L.allCheckThreatSquares (opponent color) board  
  -- встанет не под шах
  && ('c', row) `notElem` L.allCheckThreatSquares (opponent color) board  
  where row = initialRow color

possibleMoves :: Board -> Color -> Square -> [Move]
possibleMoves board color square = castlings ++ captures ++ moves
  where
    captures = map (Capture (piece color) square) $ captureThreatSquares color square board
    -- FIMXE: вынести во внешнюю функцию
    moveSquares = filter (`notElem` captureThreatSquares color square board) $ underAttackSquares board color square
    moves    = map (Move (piece color) square) moveSquares
    castlings = kingsideCastling ++ queensideCastling
    kingsideCastling = [KingsideCastling color | canCastleKingside board color]
    queensideCastling = [QueensideCastling color | canCastleQueenside board color]
