module Board where

import qualified Data.List as L
import qualified Data.Map  as M
import qualified Data.Text as T
import Format.FEN (exportToFENWithoutMoveNumbers)

data PieceType = King | Queen | Rook | Bishop | Knight | Pawn deriving (Eq, Read, Show)
data Color = White | Black deriving (Eq, Read, Show)
data Piece = Piece PieceType Color deriving (Eq, Read, Show)

kingWhite :: Piece
kingWhite   = Piece King White
queenWhite :: Piece
queenWhite  = Piece Queen White
rookWhite :: Piece
rookWhite   = Piece Rook White
bishopWhite :: Piece
bishopWhite = Piece Bishop White
knightWhite :: Piece
knightWhite = Piece Knight White
pawnWhite :: Piece
pawnWhite   = Piece Pawn White
kingBlack :: Piece
kingBlack   = Piece King Black
queenBlack :: Piece
queenBlack  = Piece Queen Black
rookBlack :: Piece
rookBlack   = Piece Rook Black
bishopBlack :: Piece
bishopBlack = Piece Bishop Black
knightBlack :: Piece
knightBlack = Piece Knight Black
pawnBlack :: Piece
pawnBlack   = Piece Pawn Black

type Square = (Char, Int)
data Board
  = Board
  { squares                 :: M.Map Square Piece
  , nextMove                :: Color
  , enPassantTarget         :: Maybe Square
  , whiteCanCastleKingside  :: Bool
  , whiteCanCastleQueenside :: Bool
  , blackCanCastleKingside  :: Bool
  , blackCanCastleQueenside :: Bool
  -- Счётчик полуходов. Число полуходов, прошедших с последнего хода пешки или
  -- взятия фигуры. Используется для определения применения "правила 50 ходов"
  -- (ничья).
  , halfmoveClock           :: Int
  --, moveNumber              :: Int              
  , history                 :: [Move]
  , fens                    :: M.Map String Int
  , result                  :: Maybe Result
  } deriving (Eq, Read, Show)


data DrawType =  Stalemate | ThreefoldRepetition | FiftyMove | DeadPosition | NotDefined deriving (Eq, Read, Show)
data Result = WhiteWon | BlackWon | Draw  DrawType deriving (Eq, Read, Show)

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
  deriving (Eq, Read, Show)

cols :: [Char]
cols = ['a'..'h']
rows :: [Int]
rows = [1..8]

emptyBoard :: Board
emptyBoard = Board M.empty White Nothing False False False False 0 [] M.empty Nothing

opponent :: Color -> Color
opponent White = Black
opponent _ = White

isOnBoard :: Square -> Bool
isOnBoard (col, row) = col `elem` cols && row `elem` rows

findPiece :: Board -> Square -> Maybe Piece
findPiece Board{..} square = M.lookup square squares

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

takenByPiece :: Piece -> Square -> Board -> Bool
takenByPiece piece square board = case findPiece board square of
  Just p -> p == piece
  _      -> False

-- Занято ли поле. Для хода пешкой вперёд, если поле занято,
-- то ходить вперёд нельзя (неважно какой фигурой занято поле).
taken :: Board -> Square -> Bool
taken board square = case findPiece board square of
  Just _  -> True
  Nothing -> False

pawnSquares :: Color -> Board -> [Square]
pawnSquares = pieceTypeSquares Pawn

knightSquares :: Color -> Board -> [Square]
knightSquares = pieceTypeSquares Knight

bishopSquares :: Color -> Board -> [Square]
bishopSquares = pieceTypeSquares Bishop

rookSquares :: Color -> Board -> [Square]
rookSquares = pieceTypeSquares Rook

queenSquares :: Color -> Board -> [Square]
queenSquares = pieceTypeSquares Queen

kingSquares :: Color -> Board -> [Square]
kingSquares = pieceTypeSquares King

pieceTypeSquares :: PieceType -> Color -> Board -> [Square]
pieceTypeSquares pieceType color Board{..}
  = foldl (\result (square, Piece pt c) -> if color == c && pt == pieceType then square:result else result) [] $ M.toList squares

--pieces :: Color -> Board -> [(Square, Piece)]
pieces :: Color -> Board -> [Piece]

pieces color board = map snd $ filter (\(s, Piece _ c) -> c == color) $ M.toList $ squares board
placePiece :: Square -> Piece -> Board -> Board
placePiece square piece board@Board{..} = board { squares = M.insert square piece squares }

deletePiece :: Square -> Board -> Board
deletePiece square board@Board{..} = board { squares = M.delete square squares }

-- FIXME: может быть первым параметром поставить Piece?
movePiece :: Square -> Square -> Piece -> Board -> Board
movePiece from to piece board = placePiece to piece $ deletePiece from board

placePieces :: [(Square, Piece)] -> Board -> Board
placePieces squaresAndPieces board = foldl (\b (square, piece) -> placePiece square piece b) board squaresAndPieces

pieceAt :: Piece -> Board -> [Square]
pieceAt (Piece pieceType color) Board{..} = map fst $ L.filter (\(square, Piece pt c) -> pt == pieceType && c == color) $ M.toList squares

kingAt :: Color -> Board -> Square
kingAt color board = head $ pieceAt (Piece King color) board

canCastleKingside :: Color -> Board -> Bool
canCastleKingside White board = whiteCanCastleKingside board
canCastleKingside Black board = blackCanCastleKingside board

canCastleQueenside :: Color -> Board -> Bool
canCastleQueenside White board = whiteCanCastleQueenside board
canCastleQueenside Black board = blackCanCastleQueenside board

disableKingsideCastling :: Color -> Board -> Board
disableKingsideCastling White board = board { whiteCanCastleKingside = False }
disableKingsideCastling Black board = board { blackCanCastleKingside = False }

disableQueensideCastling :: Color -> Board -> Board
disableQueensideCastling White board = board { whiteCanCastleQueenside = False }
disableQueensideCastling Black board = board { blackCanCastleQueenside = False }

disableCastling :: Color -> Board -> Board
disableCastling c = disableQueensideCastling c . disableKingsideCastling c

-- Стоят ли пешки соперника для взятие на проходе на данном поле
pawnsToEnPassantAt :: Square -> Color -> Board -> Bool
pawnsToEnPassantAt (col, row) color board = takenByPiece pawn (pred col, pawnRow) board || takenByPiece pawn (succ col, pawnRow) board
  where
    pawn = Piece Pawn color
    pawnRow = if color == White then row - 1 else row + 1

moveColor :: Move -> Color
moveColor (QueensideCastling color)              = opponent color
moveColor (KingsideCastling color)               = opponent color
moveColor (Move (Piece _ color) _ _)             = opponent color
moveColor (Capture (Piece _ color) _ _)          = opponent color
moveColor (EnPassantCapture (Piece _ color) _ _) = opponent color
moveColor (Promotion _ _ (Piece _ color))        = opponent color
moveColor (CapturePromotion _ _ (Piece _ color)) = opponent color

move :: Board -> Move -> Board
move b@Board{..} m = newBoard { fens = newFens }
  where
    newBoard = doMove b
      { enPassantTarget = Nothing
      , nextMove        = moveColor m
      , history         = m : history
      } m
    newFens = M.insertWith (+) (exportToFENWithoutMoveNumbers newBoard) 1 fens

doMove :: Board -> Move -> Board
doMove board@Board{..} (Move piece@(Piece Pawn White) from@(_, 2) to@(col, 4))
  = board
  { squares = M.delete from $ M.insert to piece squares
  , enPassantTarget = if pawnsToEnPassantAt (col, 3) Black board then Just (col, 3) else Nothing
  , halfmoveClock = 0
  }

doMove board@Board{..} (Move piece@(Piece Pawn Black) from@(_, 7) to@(col, 5))
  = board
  { squares = M.delete from $ M.insert to piece squares
  , enPassantTarget = if pawnsToEnPassantAt (col, 6) White board then Just (col, 6) else Nothing
  , halfmoveClock = 0
  }

doMove board@Board{..} (Move piece@(Piece King color) from to)
  = disableCastling color
  $ board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook White) from@('h', 1) to)
  = disableKingsideCastling White
  $ board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook White) from@('a', 1) to)
  = disableQueensideCastling White
  $ board { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook Black) from@('h', 8) to)
  = disableKingsideCastling Black
  $ board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook Black) from@('a', 8) to)
  = disableQueensideCastling Black
  $ board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece pt _) from to)
  = board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = if pt == Pawn then 0 else halfmoveClock + 1
  }

doMove board@Board{..} (Capture   piece  from to)
  = board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (Promotion        from to piece)
  = board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (CapturePromotion from to piece)
  = board
  { squares = M.delete from $ M.insert to piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (EnPassantCapture piece from@(_, fRow) to@(tCol, _))
  = board
  { squares = M.delete (tCol, fRow) $ M.delete from $ M.insert to piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (KingsideCastling color)
  = disableCastling color $ board
  { squares
      = M.insert ('f', row) (Piece Rook color) $ M.delete ('h', row)
      $ M.insert ('g', row) (Piece King color) $ M.delete ('e', row) squares
  , halfmoveClock = halfmoveClock + 1
  }
  where row = if color == White then 1 else 8

doMove board@Board{..} (QueensideCastling color)
  = disableCastling color $ board
  { squares
      = M.insert ('d', row) (Piece Rook color) $ M.delete ('a', row)
      $ M.insert ('c', row) (Piece King color) $ M.delete ('e', row) squares
  , halfmoveClock = halfmoveClock + 1
  }
  where row = if color == White then 1 else 8

applyMoves :: Board -> [Move] -> Board
applyMoves = foldl move