module Laws where

import           Data.Char ( ord, chr )
import           Board

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

whitePawnMoves :: Square -> [Square]
whitePawnMoves (col, row) | row == 2 =           [(col, 3), (col, 4)]
                          | row > 2 && row < 8 = [(col, succ row)]
                          | otherwise = undefined

blackPawnMoves :: Square -> [Square]
blackPawnMoves (col, row) | row == 7 =           [(col, 6), (col, 5)]
                          | row < 7 && row > 1 = [(col, pred row)]
                          | otherwise = undefined

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists = filter (not . null)

whitePawnCaptureSquares :: Square -> [Square]
whitePawnCaptureSquares ('a', row) = [('b', succ row)]
whitePawnCaptureSquares ('h', row) = [('g', succ row)]
whitePawnCaptureSquares (col, row) = [(pred col, succ row), (succ col, succ row)]

whitePawnCaptures :: Board -> Square -> [Move]
whitePawnCaptures board from@(_, row) | row == 7  = [f piece | f <- capturePromotions, piece <- [queenWhite, rookWhite, bishopWhite, knightWhite]]
                                      | otherwise = captures
  where
    capturePromotions = map (\to -> CapturePromotion from to) blackPieceSquares
    captures = map (\to -> Capture pawnWhite from to) blackPieceSquares
    blackPieceSquares = filter (takenByBlacks board) $ whitePawnCaptureSquares from

whitePawnPossibleMoves :: Square -> Board -> [Move]
whitePawnPossibleMoves from@(_, row) board = whitePawnCaptures board from ++ moves
  where
    moves = map (\to -> Move pawnWhite from to) $ whitePawnMoves from
