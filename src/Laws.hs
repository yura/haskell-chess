module Laws where

import           Data.Char ( ord, chr )
import           Board
import           Laws.Pawn

bottomLeft :: Square -> [Square]
bottomLeft (col, row)  = zip [pred col, pred (pred col)..head cols] [pred row, pred (pred row)..head rows]

bottomRight :: Square -> [Square]
bottomRight (col, row) = zip [succ col..last cols] [pred row, pred (pred row)..head rows]

topRight :: Square -> [Square]
topRight (col, row)    = zip [succ col.. last cols] [succ row..last rows]

topLeft :: Square -> [Square]
topLeft (col, row)     = zip [pred col, pred (pred col)..head cols] [succ row..last rows]

bishopMovesGrouped :: Square -> [[Square]]
bishopMovesGrouped squareName = dropEmptyLists $ bottomLeft squareName : bottomRight squareName : topRight squareName : topLeft squareName : []

bishopMoves :: Square -> [Square]
bishopMoves = concat . bishopMovesGrouped

knightMoveShifts :: [(Int, Int)]
knightMoveShifts = [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

knightMoves :: Square -> [Square]
knightMoves (col, row) = filter isOnBoard $ map (\(c', r') -> (chr (ord col + c'), row + r')) knightMoveShifts

leftDirection :: Square -> [Square]
leftDirection (col, row) = map (\c -> (c, row)) [pred col, pred (pred col)..head cols]

downDirection :: Square -> [Square]
downDirection (col, row) = map (\r -> (col, r)) [pred row, pred (pred row)..head rows]

rightDirection :: Square -> [Square]
rightDirection (col, row) = map (\c -> (c, row)) [succ col..last cols]

upDirection :: Square -> [Square]
upDirection (col, row) = map (\r -> (col, r)) [succ row..last rows]

rookMovesGrouped :: Square -> [[Square]]
rookMovesGrouped squareName = dropEmptyLists $ leftDirection squareName : downDirection squareName : rightDirection squareName : upDirection squareName : []

rookMoves :: Square -> [Square]
rookMoves = concat . rookMovesGrouped

queenMoves :: Square -> [Square]
queenMoves squareName = bishopMoves squareName ++ rookMoves squareName

kingMoves :: Square -> [Square]
kingMoves squareName = map (\group -> head group) (bishopMovesGrouped squareName ++ rookMovesGrouped squareName) -- ++ rookMovesGrouped squareName)

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists = filter (not . null)

possibleMoves :: Color -> Board -> [Move]
possibleMoves color board = concatMap (\s -> pawnPossibleMoves color s board) $ pawnSquares color board

isMate :: Color -> Board -> Bool
isMate = undefined
