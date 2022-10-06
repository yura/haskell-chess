module Board where

import Debug.Trace

import           Control.Lens
import           Data.Char ( chr, digitToInt, ord )
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Maybe ( fromJust )
import qualified Data.Text as T
import           Text.Read

import           Format.FEN ( exportToFENWithoutMoveNumbers )

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

newtype Square = Square Int deriving (Eq, Ord)

instance Read Square where
  --readPrec = do
  --  file <- readPrec
  --  rank <- readPrec
  --  return $ traceShow (file, rank) $ Square $ (digitToInt rank - 1) * 8 + (ord file - 97)
  readsPrec _ (col:row:xs) = [(Square $ (digitToInt row - 1) * 8 + (ord col - 97), xs)]
  readsPrec _ _ = []

instance Show Square where
  show square = (chr $ squareFile square + 97) : show (squareRank square + 1)

data Board
  = Board
  { squares                 :: [Maybe Piece]
  , nextMove                :: Color
  , enPassantTarget         :: Maybe (Square, Square)
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

squareFile :: Square -> Int
squareFile (Square s) = s `mod` 8

squareRank :: Square -> Int
squareRank (Square s) = s `div` 8

firstFile :: Int
firstFile = 0

lastFile :: Int
lastFile = 7

firstRank :: Int
firstRank = 0

lastRank :: Int
lastRank = 7

emptyBoard :: Board
emptyBoard = Board (replicate 64 Nothing) White Nothing False False False False 0 1 [] M.empty Nothing

opponent :: Color -> Color
opponent White = Black
opponent _ = White

isOnBoard :: Square -> Bool
isOnBoard (Square s) = s >= 0 && s < 64

findPiece :: Board -> Square -> Maybe Piece
findPiece Board{..} (Square s) = squares !! s

takenBy :: Color -> Board -> Square -> Bool
takenBy color Board{..} (Square s) = case squares !! s of
  Just (Piece _ c) -> color == c
  Nothing          -> False

takenByPiece :: Piece -> Square -> Board -> Bool
takenByPiece piece (Square s) Board{..} = case squares !! s of
  Just p -> p == piece
  _      -> False

-- Занято ли поле. Для хода пешкой вперёд, если поле занято,
-- то ходить вперёд нельзя (неважно какой фигурой занято поле).
taken :: Board -> Square -> Bool
taken Board{..} (Square s) = case (squares !! s) of
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
  = map Square $ L.elemIndices (Just $ Piece pieceType color) squares
  
pieces :: Color -> Board -> [(Square, Piece)]
pieces color Board{..} = foldl (filterByColor color) [] $ zip [0..] squares

filterByColor :: Color -> [(Square, Piece)] -> (Int, Maybe Piece) -> [(Square, Piece)]
filterByColor _     result (_, Nothing)                = result
filterByColor color result (s, Just p@(Piece _ c)) | c == color = (Square s, p) : result
                                                   | otherwise  = result

insertAt :: Square -> Piece -> [Maybe Piece] -> [Maybe Piece]
insertAt (Square s) piece squares = squares & element s .~ Just piece

deleteAt :: Square -> [Maybe Piece] -> [Maybe Piece]
deleteAt (Square s) squares = squares & element s .~ Nothing

placePiece :: Square -> Piece -> Board -> Board
placePiece square piece board@Board{..} = board { squares = insertAt square piece squares }

deletePiece :: Square -> Board -> Board
deletePiece square board@Board{..} = board { squares = deleteAt square squares }

-- FIXME: может быть первым параметром поставить Piece?
movePiece :: Square -> Square -> Piece -> Board -> Board
movePiece from to piece board = placePiece to piece $ deletePiece from board

placePieces :: [(Square, Piece)] -> Board -> Board
placePieces squaresAndPieces board = foldl (\b (square, piece) -> placePiece square piece b) board squaresAndPieces

pieceAt :: Piece -> Board -> [Square]
pieceAt piece Board{..} = foldl (filterPiece piece) [] $ zip [0..] squares 
  where
    --filterPiece :: Piece -> [Square] -> (Int, Maybe Piece) -> [Square]
    filterPiece _                       result (_, Nothing)               = result
    filterPiece (Piece pieceType color) result (s, Just (Piece pt c)) | pt == pieceType && c == color = Square s : result
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

increaseHalfmoveClock :: Board -> Board
increaseHalfmoveClock board@Board{..} = board { halfmoveClock = halfmoveClock + 1 }

zeroHalfmoveClock :: Board -> Board
zeroHalfmoveClock board@Board{..} = board { halfmoveClock = 0 }

setEnPassantIfCan :: Square -> Square -> Board -> Board
setEnPassantIfCan from@(Square f) to@(Square t) board
  = if twoSquareMove && pawnsToEnPassantAt target board
    then board { enPassantTarget = Just (target, to) }
    else board
  where
    twoSquareMove = abs (f - t) > 8
    target = Square $ if f < 16 then f + 8 else f - 8

-- Стоят ли пешки соперника для взятие на проходе на данном поле
-- См. раздел правил https://en.wikipedia.org/wiki/Forsyth%E2%80%93Edwards_Notation#cite_note-8
pawnsToEnPassantAt :: Square -> Board -> Bool
pawnsToEnPassantAt target@(Square t) board
  | file == firstFile = findPiece board nextFileSquare == Just opponentPawn
  | file == lastFile  = findPiece board prevFileSquare == Just opponentPawn
  | otherwise         = findPiece board nextFileSquare == Just opponentPawn || findPiece board prevFileSquare == Just opponentPawn
  where
    opponentColor = if t < 24 then Black else White
    file = squareFile target
    nextFileSquare = Square $ t + if opponentColor == White then (-7) else 9
    prevFileSquare = Square $ t + if opponentColor == White then (-9) else 7
    opponentPawn = Piece Pawn opponentColor

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
doMove board (Move piece@(Piece Pawn _) from to)
  = zeroHalfmoveClock
  $ setEnPassantIfCan from to 
  $ movePiece from to piece board

doMove board (Move piece@(Piece King color) from to)
  = increaseHalfmoveClock 
  $ disableCastling color 
  $ movePiece from to piece board

doMove board@Board{..} (Move piece@(Piece Rook White) from@(Square f) to)
  = increaseHalfmoveClock
  $ (if f == 0 then disableQueensideCastling White else id)
  $ (if f == 7 then disableKingsideCastling White else id)
  $ movePiece from to piece board

doMove board@Board{..} (Move piece@(Piece Rook Black) from@(Square f) to)
  = increaseHalfmoveClock
  $ (if f == 56 then disableQueensideCastling Black else id)
  $ (if f == 63 then disableKingsideCastling Black else id)
  $ movePiece from to piece board

doMove board (Move piece from to)
  = increaseHalfmoveClock $ movePiece from to piece board

doMove board (Capture piece  from to)
  = zeroHalfmoveClock
  $ movePiece from to piece board

doMove board (Promotion from to piece)
  = zeroHalfmoveClock
  $ movePiece from to piece board

doMove board (CapturePromotion from to piece)
  = zeroHalfmoveClock
  $ movePiece from to piece board

doMove board@Board{..} (EnPassantCapture piece from to)
  = zeroHalfmoveClock
  $ deletePiece epTo
  $ movePiece from epTarget piece board
  where
    (epTarget, epTo) = fromJust $ enPassantTarget

doMove board (KingsideCastling color)
  = increaseHalfmoveClock
  $ disableCastling color
  $ movePiece rookSquare rookSquareAfterCastling rook
  $ movePiece kingSquare kingSquareAfterCastling king board
  where
    king = Piece King color
    rook = Piece Rook color
    kingSquare = Square $ if color == White then 4 else 60
    rookSquare = Square $ if color == White then 7 else 63
    kingSquareAfterCastling = Square $ if color == White then 6 else 62
    rookSquareAfterCastling = Square $ if color == White then 5 else 61

doMove board (QueensideCastling color)
  = increaseHalfmoveClock
  $ disableCastling color
  $ movePiece rookSquare rookSquareAfterCastling rook
  $ movePiece kingSquare kingSquareAfterCastling king board
  where
    king = Piece King color
    rook = Piece Rook color
    kingSquare = Square $ if color == White then 4 else 60
    rookSquare = Square $ if color == White then 0 else 56
    kingSquareAfterCastling = Square $ if color == White then 2 else 58
    rookSquareAfterCastling = Square $ if color == White then 3 else 59

applyMoves :: Board -> [Move] -> Board
applyMoves = foldl move
