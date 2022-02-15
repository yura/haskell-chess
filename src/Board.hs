module Board where

import qualified Data.List as L
import qualified Data.Map  as M

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Show)
data Color = White | Black deriving (Eq, Show)
data Piece = Piece PieceType Color deriving (Eq, Show)

kingWhite   = Piece King White
queenWhite  = Piece Queen White
rookWhite   = Piece Rook White
bishopWhite = Piece Bishop White
knightWhite = Piece Knight White
pawnWhite   = Piece Pawn White
kingBlack   = Piece King Black
queenBlack  = Piece Queen Black
rookBlack   = Piece Rook Black
bishopBlack = Piece Bishop Black
knightBlack = Piece Knight Black
pawnBlack   = Piece Pawn Black

type Square = (Char, Int)
data Board = Board (M.Map Square Piece) (Maybe Square) deriving (Eq, Show)

enPassantTarget (Board _ target) = target

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
  -- взятие на проходе
  | EnPassantCapture  Piece Square Square
  -- превращение пешки
  | Promotion               Square Square Piece
  -- взятие с последующим превращением пешки
  | CapturePromotion        Square Square Piece
  deriving (Eq, Show)

cols = ['a'..'h']
rows = [1..8]

emptyBoard :: Board
emptyBoard = Board (M.fromList []) Nothing

opponent :: Color -> Color
opponent White = Black
opponent _ = White

isOnBoard :: Square -> Bool
isOnBoard (col, row) = col `elem` cols && row `elem` rows

findPiece :: Board -> Square -> Maybe Piece
findPiece (Board squares _) square = M.lookup square squares

takenBy :: Color -> Board -> Square -> Bool
takenBy White = takenByWhites
takenBy Black = takenByBlacks

takenByWhites :: Board -> Square -> Bool
takenByWhites board square = case findPiece board square of
  Just (Piece _ White) -> True
  _                    -> False

takenByBlacks :: Board -> Square -> Bool
takenByBlacks board square =  case findPiece board square of
  Just (Piece _ Black) -> True
  _                    -> False

-- Занято ли поле. Для хода пешкой вперёд, если поле занято,
-- то ходить вперёд нельзя (неважно какой фигурой занято поле).
taken :: Board -> Square -> Bool
taken board square = case findPiece board square of
  Just _  -> True
  Nothing -> False

pawnSquares :: Color -> Board -> [Square]
pawnSquares color (Board squares _)
  = foldl (\result (square, Piece pt c) -> if color == c && pt == Pawn then square:result else result) [] $ M.toList squares

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece (Board squares enPassantTarget) = Board (M.insert square piece squares) enPassantTarget

deletePiece :: Square -> Board -> Board
deletePiece square (Board squares enPassantTarget) = Board (M.delete square squares) enPassantTarget

-- FIXME: может быть первым параметром поставить Piece?
movePiece :: Square -> Square -> Piece -> Board -> Board
movePiece from to piece board = placePiece to piece $ deletePiece from board

placePieces :: [(Square, Piece)] -> Board -> Board
placePieces squaresAndPieces board = foldl (\b (square, piece) -> placePiece square piece b) board squaresAndPieces

pieceAt :: Piece -> Board -> [Square]
pieceAt (Piece pieceType color) (Board squares _) = map fst $ L.filter (\(square, Piece pt c) -> pt == pieceType && c == color) $ M.toList squares

kingAt :: Color -> Board -> Square
kingAt color board = head $ pieceAt (Piece King color) board

move :: Board -> Move -> Board
move (Board board _) (Move piece@(Piece Pawn White) from@(_, 2) to@(col, 4)) = Board (M.delete from $ M.insert to piece board) $ Just (col, 3)
move (Board board _) (Move piece@(Piece Pawn Black) from@(_, 7) to@(col, 5)) = Board (M.delete from $ M.insert to piece board) $ Just (col, 6)
move (Board board _) (Move piece from to)                     = Board (M.delete from $ M.insert to piece board) Nothing
move (Board board _) (Capture   piece  from to) = Board (M.delete from $ M.insert to piece board) Nothing
move (Board board _) (Promotion        from to piece)         = Board (M.delete from $ M.insert to piece board) Nothing
move (Board board _) (CapturePromotion from to piece)         = Board (M.delete from $ M.insert to piece board) Nothing
move (Board board _) (QueensideCastling color)
  = Board 
  ( M.insert ('f', row) (Piece Rook color) $ M.delete ('h', row)
  $ M.insert ('g', row) (Piece King color) $ M.delete ('e', row) board) Nothing
  where row = if color == White then 1 else 8
move (Board board _) (KingsideCastling color)
  = Board
  ( M.insert ('d', row) (Piece Rook color) $ M.delete ('a', row)
  $ M.insert ('c', row) (Piece King color) $ M.delete ('e', row) board) Nothing
  where row = if color == White then 1 else 8
