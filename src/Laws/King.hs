module Laws.King where

import           Data.Maybe ( mapMaybe )
import           Board
import qualified Laws.Queen as Q
import           Laws.Util

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

possibleMoves :: Board -> Color -> Square -> [Move]
possibleMoves board color square = captures ++ moves
  where
    captures = map (Capture (piece color) square) $ captureThreatSquares color square board
    -- FIMXE: вынести во внешнюю функцию
    moveSquares = filter (`notElem` captureThreatSquares color square board) $ underAttackSquares board color square
    moves    = map (Move (piece color) square) moveSquares
