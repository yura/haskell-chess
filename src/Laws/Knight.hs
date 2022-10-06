module Laws.Knight where

import           Data.Char ( ord, chr )
import           Board
import           Laws.Util
import Laws.Pawn (moves)

-- Ходы и возможные взятия конём
knightMoveSquareIndices :: [[Int]]
knightMoveSquareIndices =
  [ [17, 10]
  , [16, 18, 11]
  , [8, 17, 19, 12]
  , [9, 18, 20, 13]
  , [10, 19, 21, 14] 
  , [11, 20, 22, 15]
  , [12, 21, 23]
  , [13, 22] 
  , [25, 18, 2]
  , [24, 26, 19, 3]
  , [16, 0, 25, 27, 20, 4]
  , [17, 1, 26, 28, 21, 5]
  , [18, 2, 27, 29, 22, 6]
  , [19, 3, 28, 30, 23, 7]
  , [20, 4, 29, 31]
  , [21, 5, 30]
  , [33, 1, 26, 10]
  , [32, 0, 34, 2, 27, 11]
  , [24, 8, 33, 1, 35, 3, 28, 12]
  , [25, 9, 34, 2, 36, 4, 29, 13]
  , [26, 10, 35, 3, 37, 5, 30, 14]
  , [27, 11, 36, 4, 38, 6, 31, 15]
  , [28, 12, 37, 5, 39, 7]
  , [29, 13, 38, 6]
  , [41, 9, 34, 18]
  , [40, 8, 42, 10, 35, 19]
  , [32, 16, 41, 9, 43, 11, 36, 20]
  , [33, 17, 42, 10, 44, 12, 37, 21]
  , [34, 18, 43, 11, 45, 13, 38, 22]
  , [35, 19, 44, 12, 46, 14, 39, 23]
  , [36, 20, 45, 13, 47, 15]
  , [37, 21, 46, 14]
  , [49, 17, 42, 26]
  , [48, 16, 50, 18, 43, 27]
  , [40, 24, 49, 17, 51, 19, 44, 28]
  , [41, 25, 50, 18, 52, 20, 45, 29]
  , [42, 26, 51, 19, 53, 21, 46, 30]
  , [43, 27, 52, 20, 54, 22, 47, 31]
  , [44, 28, 53, 21, 55, 23]
  , [45, 29, 54, 22]
  , [57, 25, 50, 34]
  , [56, 24, 58, 26, 51, 35]
  , [48, 32, 57, 25, 59, 27, 52, 36]
  , [49, 33, 58, 26, 60, 28, 53, 37]
  , [50, 34, 59, 27, 61, 29, 54, 38]
  , [51, 35, 60, 28, 62, 30, 55, 39]
  , [52, 36, 61, 29, 63, 31]
  , [53, 37, 62, 30]
  , [33, 58, 42]
  , [32, 34, 59, 43]
  , [56, 40, 33, 35, 60, 44]
  , [57, 41, 34, 36, 61, 45]
  , [58, 42, 35, 37, 62, 46]
  , [59, 43, 36, 38, 63, 47]
  , [60, 44, 37, 39]
  , [61, 45, 38]
  , [41, 50]
  , [40, 42, 51]
  , [48, 41, 43, 52]
  , [49, 42, 44, 53]
  , [50, 43, 45, 54]
  , [51, 44, 46, 55]
  , [52, 45, 47]
  , [53, 46]
  ]

knightMoves :: Square -> [Square]
knightMoves (Square s) = map Square $ knightMoveSquareIndices !! s

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
