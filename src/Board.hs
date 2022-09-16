module Board where

import           Data.Char ( chr, ord )
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V
import Format.FEN (exportToFENWithoutMoveNumbers)

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq, Ord, Read, Show)
data Color = White | Black deriving (Eq, Ord, Read, Show)
data Piece = Piece PieceType Color deriving (Eq, Ord, Read, Show)

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
  { squares                 :: V.Vector (Maybe Piece)
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
  , moveNumber              :: Int
  , history                 :: [Move]
  , fens                    :: M.Map String Int
  , result                  :: Maybe Result
  } deriving (Eq, Read, Show)


data DrawType =  Stalemate | ThreefoldRepetition | FiftyMove | DeadPosition | NotDefined deriving (Eq, Read, Show)
data Result = WhiteWon | BlackWon | Draw  DrawType deriving (Eq, Read, Show)

data Move
  = Move             Piece Square Square
  -- рокировка в сторону ферзя
  | QueensideCastling Color
  -- рокировка в сторону короля
  | KingsideCastling  Color
  -- обычнй ход
  -- взятие
  | Capture           Piece Square Square
  -- взятие на проходе
  | EnPassantCapture  Piece Square Square
  -- превращение пешки
  | Promotion               Square Square Piece
  -- взятие с последующим превращением пешки
  | CapturePromotion        Square Square Piece
  deriving (Eq, Ord, Read, Show)

cols :: [Char]
cols = ['a'..'h']
rows :: [Int]
rows = [1..8]

emptyBoard :: Board
emptyBoard = Board (V.replicate 64 Nothing) White Nothing False False False False 0 1 [] M.empty Nothing

opponent :: Color -> Color
opponent White = Black
opponent _ = White

isOnBoard :: Square -> Bool
--isOnBoard (col, row) = col `elem` cols && row `elem` rows
isOnBoard (col, row) = col >= 'a' && col <= 'h' && row >= 1 && row <= 8 

findPiece :: Board -> Square -> Maybe Piece
findPiece Board{..} square = squares V.! (squareToIndex square)

squareToIndex :: Square -> Int
squareToIndex (col, row) = (row - 1) * 8 + (ord col - 97)

indexToSquare :: Int -> Square
indexToSquare index = (chr $ c + 97, r + 1)
  where
    (r, c) = index `divMod` 8

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

pawnSquares :: Color -> Board -> V.Vector Int
pawnSquares = pieceTypeSquares Pawn

knightSquares :: Color -> Board -> V.Vector Int
knightSquares = pieceTypeSquares Knight

bishopSquares :: Color -> Board -> V.Vector Int
bishopSquares = pieceTypeSquares Bishop

rookSquares :: Color -> Board -> V.Vector Int
rookSquares = pieceTypeSquares Rook

queenSquares :: Color -> Board -> V.Vector Int
queenSquares = pieceTypeSquares Queen

kingSquares :: Color -> Board -> V.Vector Int
kingSquares = pieceTypeSquares King

pieceTypeSquares :: PieceType -> Color -> Board -> V.Vector Int
pieceTypeSquares pieceType color Board{..}
  = V.elemIndices (Just $ Piece pieceType color) squares
  -- V.findIndices (\(Piece pt c) -> color == c && pt == pieceType) squares
  
pieces :: Color -> Board -> [(Square, Piece)]
pieces color Board{..} = V.ifoldl (filterByColor color) [] squares

filterByColor :: Color -> [(Square, Piece)] -> Int -> Maybe Piece -> [(Square, Piece)]
filterByColor _     result _     Nothing              = result
filterByColor color result index (Just p@(Piece _ c)) | c == color = (indexToSquare index, p) : result
                                                      | otherwise  = result

insertAt :: Int -> Piece -> V.Vector (Maybe Piece) -> V.Vector (Maybe Piece)
insertAt index piece squares = squares V.// [ (index, Just piece) ]

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece board@Board{..} = board { squares = insertAt (squareToIndex square) piece squares }

deleteAt :: Int -> V.Vector (Maybe Piece) -> V.Vector (Maybe Piece)
deleteAt index squares = squares V.// [ (index, Nothing) ] 

deletePiece :: Square -> Board -> Board
deletePiece square board@Board{..} = board { squares = deleteAt (squareToIndex square) squares }

-- FIXME: может быть первым параметром поставить Piece?
movePiece :: Square -> Square -> Piece -> Board -> Board
movePiece from to piece board = placePiece to piece $ deletePiece from board

placePieces :: [(Square, Piece)] -> Board -> Board
placePieces squaresAndPieces board = foldl (\b (square, piece) -> placePiece square piece b) board squaresAndPieces

pieceAt :: Piece -> Board -> [Square]
pieceAt piece Board{..} = V.ifoldl (filterPiece piece) [] squares
  where
    --filterPiece :: Piece -> [Square] -> Int -> Maybe Piece -> Square
    filterPiece _                       result _     Nothing             = result
    filterPiece (Piece pieceType color) result index (Just (Piece pt c)) | pt == pieceType && c == color = indexToSquare index : result
                                                                         | otherwise = result

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
      , nextMove        = newNextMove
      , history         = m : history
      , moveNumber      = if newNextMove == White && not (null history) then moveNumber + 1 else moveNumber
      } m
    newFens = M.insertWith (+) (exportToFENWithoutMoveNumbers newBoard) 1 fens
    newNextMove = moveColor m

doMove :: Board -> Move -> Board
doMove board@Board{..} (Move piece@(Piece Pawn White) from@(_, 2) to@(col, 4))
  = board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , enPassantTarget = if pawnsToEnPassantAt (col, 3) Black board then Just (col, 3) else Nothing
  , halfmoveClock = 0
  }

doMove board@Board{..} (Move piece@(Piece Pawn Black) from@(_, 7) to@(col, 5))
  = board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , enPassantTarget = if pawnsToEnPassantAt (col, 6) White board then Just (col, 6) else Nothing
  , halfmoveClock = 0
  }

doMove board@Board{..} (Move piece@(Piece King color) from to)
  = disableCastling color
  $ board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook White) from@('h', 1) to)
  = disableKingsideCastling White
  $ board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook White) from@('a', 1) to)
  = disableQueensideCastling White
  $ board { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook Black) from@('h', 8) to)
  = disableKingsideCastling Black
  $ board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece Rook Black) from@('a', 8) to)
  = disableQueensideCastling Black
  $ board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = halfmoveClock + 1
  }

doMove board@Board{..} (Move piece@(Piece pt _) from to)
  = board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = if pt == Pawn then 0 else halfmoveClock + 1
  }

doMove board@Board{..} (Capture   piece  from to)
  = board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (Promotion        from to piece)
  = board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (CapturePromotion from to piece)
  = board
  { squares = deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (EnPassantCapture piece from@(_, fRow) to@(tCol, _))
  = board
  { squares = deleteAt (squareToIndex (tCol, fRow)) $ deleteAt (squareToIndex from) $ insertAt (squareToIndex to) piece squares
  , halfmoveClock = 0
  }

doMove board@Board{..} (KingsideCastling color)
  = disableCastling color $ board
  { squares
      = insertAt (squareToIndex ('f', row)) (Piece Rook color) $ deleteAt (squareToIndex ('h', row))
      $ insertAt (squareToIndex ('g', row)) (Piece King color) $ deleteAt (squareToIndex ('e', row)) squares
  , halfmoveClock = halfmoveClock + 1
  }
  where row = if color == White then 1 else 8

doMove board@Board{..} (QueensideCastling color)
  = disableCastling color $ board
  { squares
      = insertAt (squareToIndex ('d', row)) (Piece Rook color) $ deleteAt (squareToIndex ('a', row))
      $ insertAt (squareToIndex ('c', row)) (Piece King color) $ deleteAt (squareToIndex ('e', row)) squares
  , halfmoveClock = halfmoveClock + 1
  }
  where row = if color == White then 1 else 8

applyMoves :: Board -> [Move] -> Board
applyMoves = foldl move
