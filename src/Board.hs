module Board where

import qualified Data.Map as M
import           Data.Text

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data PieceColor = White | Black deriving (Eq, Show)
data Piece = Piece PieceType PieceColor deriving (Eq, Show)

data Square = Square Text (Maybe Piece) deriving (Eq, Show)
data Board = Board (M.Map Text Square)

squareName :: Char -> Int -> Text
squareName row col = singleton row <> pack (show col)

squareNames = [squareName row col | row <- ['a'..'h'], col <- [1..8]]

emptyBoard :: Board
emptyBoard = Board $ M.fromList rows
  where
    rows = Prelude.map (\squareName -> (squareName, Square squareName Nothing)) squareNames

placePiece :: Text -> Piece -> Board -> Board
placePiece squareName piece (Board squares) = Board $ M.insert squareName (Square squareName $ Just piece) squares


