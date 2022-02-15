module Laws.King where

import           Board
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R

-- Геометрически возможные ходы (по пустой доске)
moveSquares :: Square -> [Square]
moveSquares square = map head (B.bishopMovesGrouped square ++ R.rookMovesGrouped square)
