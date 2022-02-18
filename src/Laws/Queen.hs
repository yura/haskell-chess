module Laws.Queen where

import           Data.Maybe (mapMaybe)
import           Board
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import           Laws.Util

pieceType :: PieceType
pieceType = Queen

piece :: Color -> Piece
piece = Piece pieceType

queenMovesGrouped :: Square -> [[Square]]
queenMovesGrouped square = B.bishopMovesGrouped square ++ R.rookMovesGrouped square

queenMoves :: Square -> [Square]
queenMoves square = B.bishopMoves square ++ R.rookMoves square

moveSquares :: Color -> Square -> Board -> [Square]
moveSquares color square board =  filter (`notElem` captureThreatSquares color square board) $ underAttackSquares board color square

underAttackSquares :: Board -> Color -> Square -> [Square]
underAttackSquares board color square = concatMap (filterAllEmptyOrFirstOpposite board color) $ queenMovesGrouped square

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = mapMaybe (findOrStop color board) (queenMovesGrouped square)

possibleMoves :: Board -> Color -> Square -> [Move]
possibleMoves board color square = captures ++ moves
  where
    captures = map (Capture (piece color) square) $ captureThreatSquares color square board
    moves    = map (Move (piece color) square)    $ moveSquares color square board
