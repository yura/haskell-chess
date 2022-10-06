module Laws.King where

import           Data.Maybe ( mapMaybe )
import           Board hiding (canCastleKingside, canCastleQueenside)
import qualified Board as B (canCastleKingside, canCastleQueenside)

import {-# SOURCE #-} qualified Laws as L
import qualified Laws.Queen as Q
import           Laws.Util

kingHome :: Color -> Square
kingHome White = Square 4
kingHome Black = Square 60

queenHome :: Color -> Square
queenHome White = Square 3
queenHome Black = Square 59

kingsideRookHome :: Color -> Square
kingsideRookHome White = Square 7
kingsideRookHome Black = Square 63

kingsideBishopHome :: Color -> Square
kingsideBishopHome White = Square 5
kingsideBishopHome Black = Square 61

kingsideKnightHome :: Color -> Square
kingsideKnightHome White = Square 6
kingsideKnightHome Black = Square 62

queensideBishopHome :: Color -> Square
queensideBishopHome White = Square 2
queensideBishopHome Black = Square 58

queensideKnightHome :: Color -> Square
queensideKnightHome White = Square 1
queensideKnightHome Black = Square 57

queensideRookHome :: Color -> Square
queensideRookHome White = Square 0
queensideRookHome Black = Square 56

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
  =  findPiece board (kingHome color) == Just (Piece King color)
  && findPiece board (kingsideRookHome color) == Just (Piece Rook color)
  -- король и ладья раньше не ходили
  && B.canCastleKingside color board
  -- нет фигур между королём и ладьёй
  && not (taken board (kingsideBishopHome color)) && not (taken board (kingsideKnightHome color))
  -- король не под шахом
  -- можно использовать isCheck:
  -- && not (isCheck color board)
  && kingHome color `notElem` L.allCheckThreatSquares (opponent color) board
  -- пройдёт не через битое поле
  && kingsideBishopHome color `notElem` L.allCheckThreatSquares (opponent color) board  
  -- встанет не под шах
  && kingsideKnightHome color `notElem` L.allCheckThreatSquares (opponent color) board  

canCastleQueenside :: Board -> Color -> Bool
canCastleQueenside board color
  =  findPiece board (kingHome color) == Just (Piece King color)
  && findPiece board (queensideRookHome color) == Just (Piece Rook color)
  -- король и ладья раньше не ходили
  && B.canCastleQueenside color board
  -- нет фигур между королём и ладьёй
  && not (taken board (queenHome color)) && not (taken board (queensideBishopHome color)) && not (taken board (queensideKnightHome color))
  -- король не под шахом
  -- можно использовать isCheck:
  -- && not (isCheck color board)
  && (kingHome color) `notElem` L.allCheckThreatSquares (opponent color) board
  -- пройдёт не через битое поле
  && (queenHome color) `notElem` L.allCheckThreatSquares (opponent color) board  
  -- встанет не под шах
  && (queensideBishopHome color) `notElem` L.allCheckThreatSquares (opponent color) board  

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
