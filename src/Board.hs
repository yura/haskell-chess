module Board where

import qualified Data.Map as M

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data PieceColor = White | Black deriving (Eq, Show)
data Piece = Piece PieceType PieceColor deriving (Eq, Show)

type SquareName = (Char, Int)
data Square = Square SquareName (Maybe Piece) deriving (Eq, Show)
data Board = Board (M.Map SquareName Square)

rowNames = ['a'..'h']
colNames = [1..8]

squareNames :: [SquareName]
squareNames = [(row, col) | row <- rowNames, col <- colNames]

emptyBoard :: Board
emptyBoard = Board $ M.fromList rows
  where
    rows = Prelude.map (\squareName -> (squareName, Square squareName Nothing)) squareNames

placePiece :: SquareName -> Piece -> Board -> Board
placePiece squareName piece (Board squares) = Board $ M.insert squareName (Square squareName $ Just piece) squares

bottomLeft :: SquareName -> [SquareName]
bottomLeft (row, col)  = zip [pred row, pred (pred row)..head rowNames] [pred col, pred (pred col)..head colNames]

bottomRight :: SquareName -> [SquareName]
bottomRight (row, col) = zip [succ row..last rowNames] [pred col, pred (pred col)..head colNames]

topRight :: SquareName -> [SquareName]
topRight (row, col)    = zip [succ row..last rowNames] [succ col.. last colNames]

topLeft :: SquareName -> [SquareName]
topLeft (row, col)     = zip [pred row, pred (pred row)..head rowNames] [succ col..last colNames]

--bishopPossibleMoves :: SquareName -> [SquareName]
--bishopPossibleMoves squareName = bottomLeft squareName ++ bottomRight squareName ++ topRight squareName ++ topLeft squareName

----
{-
squareName :: (Char, Int) -> Text
squareName (row, col) = singleton row <> pack (show col)
-}

