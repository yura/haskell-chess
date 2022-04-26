module Laws where

import Board

possibleMoves :: Board -> [Move]

-- Фигуры соперника, которые находятся под угрозой взятия.
captureThreatSquares :: Piece -> Square -> Board -> [Square]

allCaptureThreatSquares :: Color -> Board -> [Square]

-- Поля, которые находятся под угрозой шаха. Если король
-- соперника находится на одном из этих полей, то ему шах
-- и ходить на эти поля королю нельзя.
checkThreatSquares :: Piece -> Square -> Board -> [Square]


allCheckThreatSquares :: Color -> Board -> [Square]

-- Допустимые ходы в соответствии с правилами:
pawnValidMoves :: Board -> Color -> Square -> [Move]

knightValidMoves :: Board -> Color -> Square -> [Move]

bishopValidMoves :: Board -> Color -> Square -> [Move]

rookValidMoves :: Board -> Color -> Square -> [Move]

queenValidMoves :: Board -> Color -> Square -> [Move]

kingValidMoves :: Board -> Color -> Square -> [Move]

kingValidMoveSquares :: Color -> Square -> Board -> [Square]

kingCaptureThreatSquares :: Color -> Square -> Board -> [Square]

isDeadPosition :: Board -> Bool 

isCheck :: Color -> Board -> Bool

isStalemate :: Board -> Bool 

isMate :: Board -> Bool

isOver :: Board -> Maybe Result 
