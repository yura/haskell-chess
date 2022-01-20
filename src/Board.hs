module Board where

import           Data.Char ( ord, chr )
import qualified Data.Map as M

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
data Piece = Piece PieceType Color deriving (Eq, Show)

type Square = (Char, Int)
data Board = Board (M.Map Square Piece)

data Result = WhiteWon | BlackWon | Draw deriving (Eq, Show)

cols = ['a'..'h']
rows = [1..8]

squareNames :: [Square]
squareNames = [(col, row) | col <- cols, row <- rows]

emptyBoard :: Board
emptyBoard = Board $ M.fromList []

isOnBoard :: Square -> Bool
isOnBoard (col, row) = col `elem` cols && row `elem` rows

placePiece :: Board -> Square -> Piece -> Board
placePiece (Board squares) square piece = Board $ M.insert square piece squares

placePieces :: Board -> [(Square, Piece)] -> Board
placePieces board squaresAndPieces = foldl (\b (square, piece) -> placePiece b square piece) board squaresAndPieces 

findPiece :: Board -> Square -> Maybe Piece
findPiece (Board squares) square = M.lookup square squares

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
