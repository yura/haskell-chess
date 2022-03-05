module Format.FEN where

import           Data.Char (isDigit, digitToInt)
import qualified Data.Text as T
import {-# SOURCE #-} Board
import           Display (squaresDisplayOrder)
import           Format.PGN.Export (exportSquareToPGN)

-- |Экспортирует доску в формат FEN
exportToFEN :: Board -> T.Text
exportToFEN board =  T.intercalate " "
  [ exportToFENWithoutMoveNumbers board
  , T.pack $ show $ halfmoveClock board
  , fullmoveNumber
  ]

exportToFENWithoutMoveNumbers :: Board -> T.Text
exportToFENWithoutMoveNumbers board = T.intercalate " "
  [ position board
  , fenNextMove board
  , castlings board
  , enPassantTargetSquare board
  ]

position :: Board -> T.Text
position board = T.intercalate "/" $ map (rowToFEN board) squaresFENOrder

-- |Имена полей на доске, упорядоченных для экспорт в FEN
squaresFENOrder :: [[Square]]
squaresFENOrder = squaresDisplayOrder

rowToFEN :: Board -> [Square] -> T.Text
rowToFEN board row = foldl (\result square -> case findPiece board square of
  Nothing    -> incrementLastNumberOrAdd1 result
  Just piece -> result <> toFEN piece) "" row

incrementLastNumberOrAdd1 "" = "1"
incrementLastNumberOrAdd1 result = if isDigit $ T.last result then incrementLastNumber result else result <> "1"

incrementLastNumber result = firstPart <> lastPart
  where
    firstPart = T.dropEnd 1 result
    lastPart = T.pack $ show $ succ $ digitToInt (T.last result)

toFEN :: Piece -> T.Text
toFEN (Piece King White)   = "K"
toFEN (Piece Queen White)  = "Q"
toFEN (Piece Rook White)   = "R"
toFEN (Piece Bishop White) = "B"
toFEN (Piece Knight White) = "N"
toFEN (Piece Pawn White)   = "P"
toFEN (Piece King Black)   = "k"
toFEN (Piece Queen Black)  = "q"
toFEN (Piece Rook Black)   = "r"
toFEN (Piece Bishop Black) = "b"
toFEN (Piece Knight Black) = "n"
toFEN (Piece Pawn Black)   = "p"

fenNextMove :: Board -> T.Text
fenNextMove Board{ nextMove = White } = "w"
fenNextMove Board{ nextMove = Black } = "b"

castlings :: Board -> T.Text
castlings board = if T.length result == 0 then "-" else result
  where
    result = whiteKingsideCastling board <> whiteQueensideCastling board <> blackKingsideCastling board <> blackQueensideCastling board

whiteKingsideCastling :: Board -> T.Text
whiteKingsideCastling board = if whiteCanCastleKingside board then "K" else ""

whiteQueensideCastling :: Board -> T.Text
whiteQueensideCastling board = if whiteCanCastleKingside board then "Q" else ""

blackKingsideCastling :: Board -> T.Text
blackKingsideCastling board = if blackCanCastleKingside board then "k" else ""

blackQueensideCastling :: Board -> T.Text
blackQueensideCastling board = if blackCanCastleKingside board then "q" else ""

enPassantTargetSquare :: Board -> T.Text
enPassantTargetSquare board = case enPassantTarget board of
  Just s -> T.pack $ exportSquareToPGN s 
  _      -> "-"

fullmoveNumber :: T.Text
fullmoveNumber = "1"

