module Laws.Knight where

import           Data.Char ( ord, chr )
import           Board
import           Laws.Util
import Laws.Pawn (moves)

-- Ходы и возможные взятия конём

knightMoveShifts :: [(Int, Int)]
knightMoveShifts = [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

knightMoves :: Square -> [Square]
knightMoves (col, row) = filter isOnBoard $ map (\(c', r') -> (chr (ord col + c'), row + r')) knightMoveShifts

-- Конь бьёт или прикрывает все поля в соответстии с возможными ходами
underAttackSquares :: Square -> [Square]
underAttackSquares = knightMoves

moveSquares :: Color -> Square -> Board -> [Square]
moveSquares color square board =  filter (not . taken board) $ knightMoves square

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board = filterThreats color board $ knightMoves square

possibleMoves :: Board -> Color -> Square -> [Move]
possibleMoves board color square = captures ++ moves
  where
    captureSquares = captureThreatSquares color square board
    captures = map (Capture (Piece Knight color) square) captureSquares
    moves = map (Move (Piece Knight color) square) $ moveSquares color square board