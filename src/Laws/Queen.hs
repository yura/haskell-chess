module Laws.Queen where

import           Data.Maybe (fromJust, isJust)
import           Board
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import           Laws.Util

queenMovesGrouped :: Square -> [[Square]]
queenMovesGrouped square = B.bishopMovesGrouped square ++ R.rookMovesGrouped square

queenMoves :: Square -> [Square]
queenMoves square = B.bishopMoves square ++ R.rookMoves square

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = map fromJust
  $ filter isJust
  $ map (findOrStop color board)
  $ queenMovesGrouped square
