module Laws.Util where

import           Data.List (find)
import           Data.Maybe (fromJust, isJust)
import           Board

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists = filter (not . null)

-- Выбирает только те поля, на которых стоят фигуры соперника
filterThreats :: Color -> Board -> [Square] -> [Square]
filterThreats color board = filter (takenBy (opponent color) board) 

findOrStop :: Color -> Board -> [Square] -> Maybe Square
findOrStop _ _ [] = Nothing
findOrStop White board (square:squares) = case findPiece board square of
  Just (Piece _ Black) -> Just square
  Just (Piece _ White) -> Nothing
  Nothing              -> findOrStop White board squares
findOrStop Black board (square:squares) = case findPiece board square of
  Just (Piece _ White) -> Just square
  Just (Piece _ Black) -> Nothing
  Nothing              -> findOrStop Black board squares

-- Выбирает все пустые поля пока не дойдёт до конца, либо до своей фигуры,
-- либо до фигуры противника, которую тоже включает в список 
filterAllEmptyOrFirstOpposite :: Board -> Color -> [Square] -> [Square]
filterAllEmptyOrFirstOpposite board color [] = []
filterAllEmptyOrFirstOpposite board color (square:squares) = case findPiece board square of
  Nothing          -> square : filterAllEmptyOrFirstOpposite board color squares
  Just (Piece _ c) -> [square | c /= color]