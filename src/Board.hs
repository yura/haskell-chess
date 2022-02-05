module Board where

import qualified Data.Map as M

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
data Piece = Piece PieceType Color deriving (Eq, Show)

type Square = (Char, Int)
data Board = Board (M.Map Square Piece) deriving (Eq, Show)

data Result = WhiteWon | BlackWon | Draw deriving (Eq, Show)

data Move
  -- рокировка в сторону ферзя
  = QueensideCastling Color
  -- рокировка в сторону короля
  | KingsideCastling  Color
  -- обычнй ход
  | Move              Piece Square Square
  -- взятие
  | Capture           Piece Square Square
  -- превращение пешки
  | Promotion               Square Square Piece
  -- взятие с последующим превращением пешки
  | CapturePromotion        Square Square Piece
  deriving (Eq, Show)

cols = ['a'..'h']
rows = [1..8]

squareNames :: [Square]
squareNames = [(col, row) | col <- cols, row <- rows]

emptyBoard :: Board
emptyBoard = Board $ M.fromList []

findPiece :: Board -> Square -> Maybe Piece
findPiece (Board squares) square = M.lookup square squares

isOnBoard :: Square -> Bool
isOnBoard (col, row) = col `elem` cols && row `elem` rows

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece (Board squares) = Board $ M.insert square piece squares

deletePiece :: Square -> Board -> Board
deletePiece square (Board squares) = Board $ M.delete square squares

movePiece :: Square -> Square -> Piece -> Board -> Board
movePiece from to piece board = placePiece to piece $ deletePiece from board

placePieces :: [(Square, Piece)] -> Board -> Board
placePieces squaresAndPieces board = foldl (\b (square, piece) -> placePiece square piece b) board squaresAndPieces 

move :: Board -> Move -> Board
move (Board board) (Move      piece  from to) = Board $ M.delete from $ M.insert to piece board
move (Board board) (Capture   piece  from to) = Board $ M.delete from $ M.insert to piece board
move (Board board) (Promotion        from to piece) = Board $ M.delete from $ M.insert to piece board
move (Board board) (CapturePromotion from to piece) = Board $ M.delete from $ M.insert to piece board
move (Board board) (QueensideCastling color)
  = Board
  $ M.insert ('f', row) (Piece Rook White) $ M.delete ('h', row)
  $ M.insert ('g', row) (Piece King White) $ M.delete ('e', row) board
  where row = if color == White then 1 else 8
move (Board board) (KingsideCastling color)
  = Board
  $ M.insert ('d', row) (Piece Rook White) $ M.delete ('a', row)
  $ M.insert ('c', row) (Piece King White) $ M.delete ('e', row) board
  where row = if color == White then 1 else 8
