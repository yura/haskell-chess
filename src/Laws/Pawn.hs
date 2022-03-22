module Laws.Pawn where

import Board
import Laws.Util

initialRow :: Color -> Int
initialRow White = 2
initialRow Black = 7

finalRow :: Color -> Int
finalRow White = 8
finalRow Black = 1

predFinalRow :: Color -> Int
predFinalRow White = 7
predFinalRow Black = 2

nextSquare :: Color -> Int -> Int
nextSquare White = succ
nextSquare Black = pred

enPassantRow :: Color -> Int
enPassantRow White = 5
enPassantRow Black = 4

moveSquares :: Color -> Square -> [Square]
moveSquares color (col, row)
  | row == initialRow color = [(col, nextSquare color row), (col, nextSquare color $ nextSquare color row)]
  | otherwise               = [(col, nextSquare color row)]

whitePawnMoveSquares :: Square -> [Square]
whitePawnMoveSquares = moveSquares White

blackPawnMoveSquares :: Square -> [Square]
blackPawnMoveSquares = moveSquares Black

captureSquares :: Color -> Square -> [Square]
captureSquares color ('a', row) = [('b', nextSquare color row)]
captureSquares color ('h', row) = [('g', nextSquare color row)]
captureSquares color (col, row) = [(pred col, nextSquare color row), (succ col, nextSquare color row)]

-- Поля, которые атакует пешка находясь в данной позиции
underAttackSquares :: Board -> Color -> Square -> [Square]
underAttackSquares _ = captureSquares

-- Угрозы взятия фигур соперника
captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board = filterThreats color board $ captureSquares color square

whitePawnCaptureSquares :: Square -> [Square]
whitePawnCaptureSquares = captureSquares White

blackPawnCaptureSquares :: Square -> [Square]
blackPawnCaptureSquares = captureSquares Black

moves :: Color -> Square -> Board -> [Move]
moves color from@(col, row) board
  | row == initialRow color && taken board (col, nextSquare color row) = [] -- этот случай рассматривается отдельно, чтобы избежать "перепрыгиваний"
  | row == predFinalRow color && taken board (col, finalRow color) = []
  | row == predFinalRow color = map (Promotion from (col, finalRow color)) [Piece Queen color, Piece Rook color, Piece Bishop color, Piece Knight color]
  | otherwise = map (Move (Piece Pawn color) from) $ filter (not . taken board) $ moveSquares color from

captures :: Color -> Square -> Board -> [Move]
captures color from@(_, row) board
  | nextSquare color row == finalRow color = [f piece | f <- capturePromotions, piece <- [Piece Queen color, Piece Rook color, Piece Bishop color, Piece Knight color]]
  | otherwise = map (Capture (Piece Pawn color) from) squares
  where
    capturePromotions = map (CapturePromotion from) squares
    squares = captureThreatSquares color from board

enPassantCapture :: Color -> Square -> Board -> [Move]
enPassantCapture color (col, row) (Board{enPassantTarget = (Just target@(targetCol, _))})
  = [EnPassantCapture (Piece Pawn color) (col, enPassantRow color) target | row == enPassantRow color && (succ targetCol == col || pred targetCol == col)]
enPassantCapture _ _ _ = []

pawnPossibleMoves :: Color -> Square -> Board -> [Move]
pawnPossibleMoves color from board
  =  enPassantCapture color from board
  ++ captures color from board
  ++ moves color from board

whitePawnPossibleMoves :: Square -> Board -> [Move]
whitePawnPossibleMoves = pawnPossibleMoves White

blackPawnPossibleMoves :: Square -> Board -> [Move]
blackPawnPossibleMoves = pawnPossibleMoves Black