module Board where

import qualified Data.Map as M
import Data.Char ( ord, chr )

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data PieceColor = White | Black deriving (Eq, Show)
data Piece = Piece PieceType PieceColor deriving (Eq, Show)

type Square = (Char, Int)
data Board = Board (M.Map Square Piece)
colNames = ['a'..'h']
rowNames = [1..8]

squareNames :: [Square]
squareNames = [(col, row) | col <- colNames, row <- rowNames]

emptyBoard :: Board
emptyBoard = Board $ M.fromList [] --rows
--  where
--    rows = Prelude.map (\squareName -> (squareName, Nothing)) squareNames

placePiece :: Board -> Square -> Piece -> Board
placePiece (Board squares) square piece = Board $ M.insert square piece squares

placePieces :: Board -> [(Square, Piece)] -> Board
placePieces board squaresAndPieces = foldl (\b (square, piece) -> placePiece b square piece) board squaresAndPieces 

findPiece :: Board -> Square -> Maybe Piece
findPiece (Board squares) square = M.lookup square squares

bottomLeft :: Square -> [Square]
bottomLeft (col, row)  = zip [pred col, pred (pred col)..head colNames] [pred row, pred (pred row)..head rowNames]

bottomRight :: Square -> [Square]
bottomRight (col, row) = zip [succ col..last colNames] [pred row, pred (pred row)..head rowNames]

topRight :: Square -> [Square]
topRight (col, row)    = zip [succ col.. last colNames] [succ row..last rowNames]

topLeft :: Square -> [Square]
topLeft (col, row)     = zip [pred col, pred (pred col)..head colNames] [succ row..last rowNames]

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists xss = filter (\xs -> length xs > 0) xss

bishopMovesGrouped :: Square -> [[Square]]
bishopMovesGrouped squareName = dropEmptyLists $ bottomLeft squareName : bottomRight squareName : topRight squareName : topLeft squareName : []

bishopMoves :: Square -> [Square]
bishopMoves = concat . bishopMovesGrouped

knightMoveShifts :: [(Int, Int)]
knightMoveShifts = [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

isOnBoard :: Square -> Bool
isOnBoard (col, row) = col >= head colNames && col <= last colNames && row >= head rowNames && row <= last rowNames

knightMoves :: Square -> [Square]
knightMoves (col, row) = filter isOnBoard $ map (\(c', r') -> (chr (ord col + c'), row + r')) knightMoveShifts

leftDirection :: Square -> [Square]
leftDirection (col, row) = map (\c -> (c, row)) [pred col, pred (pred col)..head colNames]

downDirection :: Square -> [Square]
downDirection (col, row) = map (\r -> (col, r)) [pred row, pred (pred row)..head rowNames]

rightDirection :: Square -> [Square]
rightDirection (col, row) = map (\c -> (c, row)) [succ col..last colNames]

upDirection :: Square -> [Square]
upDirection (col, row) = map (\r -> (col, r)) [succ row..last rowNames]

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
