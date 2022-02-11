module Laws.Knight where

import           Data.Char ( ord, chr )
import           Board
import           Laws.Util

-- Ходы и возможные взятия конём

knightMoveShifts :: [(Int, Int)]
knightMoveShifts = [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

knightMoves :: Square -> [Square]
knightMoves (col, row) = filter isOnBoard $ map (\(c', r') -> (chr (ord col + c'), row + r')) knightMoveShifts

captureThreats :: Color -> Square -> Board -> [Square]
captureThreats color square board = filterThreats color board $ knightMoves square

