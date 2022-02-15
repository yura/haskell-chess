module Laws.Knight where

import           Data.Char ( ord, chr )
import           Board
import           Laws.Util

-- Ходы и возможные взятия конём

knightMoveShifts :: [(Int, Int)]
knightMoveShifts = [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

knightMoves :: Square -> [Square]
knightMoves (col, row) = filter isOnBoard $ map (\(c', r') -> (chr (ord col + c'), row + r')) knightMoveShifts

-- Конь бьёт все поля в соответстии с возможными ходами
underAttackSquares :: Square -> [Square]
underAttackSquares = knightMoves

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board = filterThreats color board $ knightMoves square

