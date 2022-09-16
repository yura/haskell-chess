module Board where

import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector as V

data PieceType = Pawn | Knight | Bishop | Rook | Queen | King
data Color = White | Black
data Piece = Piece PieceType Color

kingWhite :: Piece
queenWhite :: Piece
rookWhite :: Piece
bishopWhite :: Piece
knightWhite :: Piece
pawnWhite :: Piece
kingBlack :: Piece
queenBlack :: Piece
rookBlack :: Piece
bishopBlack :: Piece
knightBlack :: Piece
pawnBlack :: Piece

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
  }  

data DrawType =  Stalemate | ThreefoldRepetition | FiftyMove | DeadPosition | NotDefined
data Result = WhiteWon | BlackWon | Draw  DrawType

data Move
  = Move              Piece Square Square
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

cols :: [Char]
rows :: [Int]

emptyBoard :: Board

opponent :: Color -> Color

isOnBoard :: Square -> Bool

findPiece :: Board -> Square -> Maybe Piece

takenBy :: Color -> Board -> Square -> Bool

takenByWhites :: Board -> Square -> Bool

takenByBlacks :: Board -> Square -> Bool

takenByPiece :: Piece -> Square -> Board -> Bool

-- Занято ли поле. Для хода пешкой вперёд, если поле занято,
-- то ходить вперёд нельзя (неважно какой фигурой занято поле).
taken :: Board -> Square -> Bool

pawnSquares :: Color -> Board -> V.Vector Int

knightSquares :: Color -> Board -> V.Vector Int

bishopSquares :: Color -> Board -> V.Vector Int

rookSquares :: Color -> Board -> V.Vector Int

queenSquares :: Color -> Board -> V.Vector Int

kingSquares :: Color -> Board -> V.Vector Int

pieceTypeSquares :: PieceType -> Color -> Board -> V.Vector Int

pieces :: Color -> Board -> [(Square, Piece)]

placePiece :: Square -> Piece -> Board -> Board

deletePiece :: Square -> Board -> Board

-- FIXME: может быть первым параметром поставить Piece?
movePiece :: Square -> Square -> Piece -> Board -> Board

placePieces :: [(Square, Piece)] -> Board -> Board

pieceAt :: Piece -> Board -> [Square]

kingAt :: Color -> Board -> Square

canCastleKingside :: Color -> Board -> Bool

canCastleQueenside :: Color -> Board -> Bool

disableKingsideCastling :: Color -> Board -> Board

disableQueensideCastling :: Color -> Board -> Board

disableCastling :: Color -> Board -> Board

-- Стоят ли пешки соперника для взятие на проходе на данном поле
pawnsToEnPassantAt :: Square -> Color -> Board -> Bool

moveColor :: Move -> Color

move :: Board -> Move -> Board

doMove :: Board -> Move -> Board

applyMoves :: Board -> [Move] -> Board
