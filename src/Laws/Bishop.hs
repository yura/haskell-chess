module Laws.Bishop where

import           Data.List (find)
import           Data.Maybe (fromJust, isJust)
import           Board
import           Laws.Util

sw :: Square -> [Square]
sw (col, row) = zip [pred col, pred (pred col)..head cols] [pred row, pred (pred row)..head rows]

se :: Square -> [Square]
se (col, row) = zip [succ col..last cols] [pred row, pred (pred row)..head rows]

ne :: Square -> [Square]
ne (col, row) = zip [succ col.. last cols] [succ row..last rows]

nw :: Square -> [Square]
nw (col, row) = zip [pred col, pred (pred col)..head cols] [succ row..last rows]

bishopMovesGrouped :: Square -> [[Square]]
-- bishopMovesGrouped square = dropEmptyLists $ sw square : se square : ne square : nw square : []
bishopMovesGrouped square = filter (not . null) $ map (\f -> f square) [sw, se, ne, nw]

bishopMoves :: Square -> [Square]
bishopMoves = concat . bishopMovesGrouped

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

bishopCaptures :: Color -> Square -> Board -> [Square]
bishopCaptures color square board
  = map fromJust
  $ filter isJust
  $ map (findOrStop color board)
  $ bishopMovesGrouped square
