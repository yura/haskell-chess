module Board where

import qualified Data.Map as M
import Data.Char ( ord, chr )

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data PieceColor = White | Black deriving (Eq, Show)
data Piece = Piece PieceType PieceColor deriving (Eq, Show)

type SquareName = (Char, Int)
data Square = Square SquareName (Maybe Piece) deriving (Eq, Show)
data Board = Board (M.Map SquareName Square)

colNames = ['a'..'h']
rowNames = [1..8]

squareNames :: [SquareName]
squareNames = [(col, row) | col <- colNames, row <- rowNames]

emptyBoard :: Board
emptyBoard = Board $ M.fromList rows
  where
    rows = Prelude.map (\squareName -> (squareName, Square squareName Nothing)) squareNames

placePiece :: SquareName -> Piece -> Board -> Board
placePiece squareName piece (Board squares) = Board $ M.insert squareName (Square squareName $ Just piece) squares

bottomLeft :: SquareName -> [SquareName]
bottomLeft (col, row)  = zip [pred col, pred (pred col)..head colNames] [pred row, pred (pred row)..head rowNames]

bottomRight :: SquareName -> [SquareName]
bottomRight (col, row) = zip [succ col..last colNames] [pred row, pred (pred row)..head rowNames]

topRight :: SquareName -> [SquareName]
topRight (col, row)    = zip [succ col.. last colNames] [succ row..last rowNames]

topLeft :: SquareName -> [SquareName]
topLeft (col, row)     = zip [pred col, pred (pred col)..head colNames] [succ row..last rowNames]

dropEmptyLists :: [[a]] -> [[a]]
dropEmptyLists xss = filter (\xs -> length xs > 0) xss

bishopMovesGrouped :: SquareName -> [[SquareName]]
bishopMovesGrouped squareName = dropEmptyLists $ bottomLeft squareName : bottomRight squareName : topRight squareName : topLeft squareName : []

bishopMoves :: SquareName -> [SquareName]
bishopMoves = concat . bishopMovesGrouped

knightMoveShifts :: [(Int, Int)]
knightMoveShifts = [(-2, -1), (-1, -2), (1, -2), (2, -1), (2, 1), (1, 2), (-1, 2), (-2, 1)]

isOnBoard :: SquareName -> Bool
isOnBoard (col, row) = col >= head colNames && col <= last colNames && row >= head rowNames && row <= last rowNames

knightMoves :: SquareName -> [SquareName]
knightMoves (col, row) = filter isOnBoard $ map (\(c', r') -> (chr (ord col + c'), row + r')) knightMoveShifts

leftDirection :: SquareName -> [SquareName]
leftDirection (col, row) = map (\c -> (c, row)) [pred col, pred (pred col)..head colNames]

downDirection :: SquareName -> [SquareName]
downDirection (col, row) = map (\r -> (col, r)) [pred row, pred (pred row)..head rowNames]

rightDirection :: SquareName -> [SquareName]
rightDirection (col, row) = map (\c -> (c, row)) [succ col..last colNames]

upDirection :: SquareName -> [SquareName]
upDirection (col, row) = map (\r -> (col, r)) [succ row..last rowNames]

rookMovesGrouped :: SquareName -> [[SquareName]]
rookMovesGrouped squareName = dropEmptyLists $ leftDirection squareName : downDirection squareName : rightDirection squareName : upDirection squareName : []

rookMoves :: SquareName -> [SquareName]
rookMoves = concat . rookMovesGrouped

queenMoves :: SquareName -> [SquareName]
queenMoves squareName = bishopMoves squareName ++ rookMoves squareName

kingMoves :: SquareName -> [SquareName]
kingMoves squareName = map (\group -> head group) (bishopMovesGrouped squareName ++ rookMovesGrouped squareName) -- ++ rookMovesGrouped squareName)
