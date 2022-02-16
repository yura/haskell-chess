module Laws.King where

import           Data.Maybe ( mapMaybe )
import           Board
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import           Laws.Util

-- Геометрически возможные ходы (по пустой доске)
moveSquares :: Square -> [Square]
moveSquares square = map head (B.bishopMovesGrouped square ++ R.rookMovesGrouped square)

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = mapMaybe (findOrStop color board . (:[])) (moveSquares square)
