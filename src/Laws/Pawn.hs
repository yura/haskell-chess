module Laws.Pawn where

import Board hiding ( lastRank )
import Laws.Util

finalRow :: Color -> Int
finalRow White = 8
finalRow Black = 1

predFinalRow :: Color -> Int
predFinalRow White = 7
predFinalRow Black = 2

nextSquare :: Color -> Square -> Square
nextSquare White (Square s) = Square $ s + 8
nextSquare Black (Square s) = Square $ s - 8

enPassantRank :: Color -> Int
enPassantRank White = 4
enPassantRank Black = 3

atHomeRank :: Color -> Square -> Bool
atHomeRank White (Square s) = s > 7 && s < 16
atHomeRank Black (Square s) = s > 47 && s < 56

atPredFinalRow :: Color -> Square -> Bool
atPredFinalRow White (Square s) = s > 47 && s < 56
atPredFinalRow Black (Square s) = s > 7  && s < 16

moveSquares :: Color -> Square -> [Square]
moveSquares color square
  | atHomeRank color square = [nextSquare color square, nextSquare color $ nextSquare color square]
  | otherwise               = [nextSquare color square]

captureSquares :: Color -> Square -> [Square]
captureSquares White square@(Square s) | file == firstFile = [Square $ s + 9] -- колонка 'a'
                                       | file == lastFile  = [Square $ s + 7] -- колонка 'h'
                                       | otherwise         = map Square [s + 7, s + 9]
  where file = squareFile square

captureSquares Black square@(Square s) | file == firstFile = [Square $ s - 7] -- колонка 'a'
                                       | file == lastFile  = [Square $ s - 9] -- колонка 'h'
                                       | otherwise         = map Square [s - 9, s - 7]
  where file = squareFile square

-- Поля, которые атакует пешка находясь в данной позиции
underAttackSquares :: Board -> Color -> Square -> [Square]
underAttackSquares _ = captureSquares

-- Угрозы взятия фигур соперника
captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board = filterThreats color board $ captureSquares color square

moves :: Color -> Square -> Board -> [Move]
moves color from board
  | atHomeRank color from && taken board (nextSquare color from) = [] -- этот случай рассматривается отдельно, чтобы избежать "перепрыгиваний"
  | atPredFinalRow color from && taken board (nextSquare color from) = []
  | atPredFinalRow color from = map (Promotion from (nextSquare color from)) [Piece Queen color, Piece Rook color, Piece Bishop color, Piece Knight color]
  | otherwise = map (Move (Piece Pawn color) from) $ filter (not . taken board) $ moveSquares color from

captures :: Color -> Square -> Board -> [Move]
captures color from board
  | atPredFinalRow color from = [f piece | f <- capturePromotions, piece <- [Piece Queen color, Piece Rook color, Piece Bishop color, Piece Knight color]]
  | otherwise = map (Capture (Piece Pawn color) from) squares
  where
    capturePromotions = map (CapturePromotion from) squares
    squares = captureThreatSquares color from board

enPassantCapture :: Color -> Square -> Board -> [Move]
enPassantCapture color from (Board{enPassantTarget = (Just (target, to))})
  = [EnPassantCapture (Piece Pawn color) from target | rankCondition && fileCondition]
  where
    rankCondition = squareRank from == enPassantRank color
    fromFile =  squareFile from
    targetFile = squareFile target
    fileCondition
      =  (targetFile == firstFile && fromFile == targetFile + 1)
      || (targetFile == lastFile  && fromFile == targetFile - 1)
      || (targetFile /= firstFile && targetFile /= lastFile && (fromFile == targetFile + 1 || fromFile == targetFile - 1))
    
enPassantCapture _ _ _ = []

pawnPossibleMoves :: Color -> Square -> Board -> [Move]
pawnPossibleMoves color from board
  =  enPassantCapture color from board
  ++ captures color from board
  ++ moves color from board


-- Для упрощения и структуризации тестов
whitePawnPossibleMoves :: Square -> Board -> [Move]
whitePawnPossibleMoves = pawnPossibleMoves White

blackPawnPossibleMoves :: Square -> Board -> [Move]
blackPawnPossibleMoves = pawnPossibleMoves Black
