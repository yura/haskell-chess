module Laws.Util where

import Board

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists = filter (not . null)

-- Выбирает только те поля, на которых стоят фигуры соперника
filterThreats :: Color -> Board -> [Square] -> [Square]
filterThreats color board = filter (takenBy (opponent color) board) 

