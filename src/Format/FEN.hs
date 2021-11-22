module Format.FEN where

import           Data.Char (isDigit, digitToInt)
import qualified Data.Text as T
import           Board

exportFEN :: Board -> T.Text
exportFEN board = T.intercalate " " [position board, nextMove, castlings board, enPassantTargetSquare, halfmoveClock, fullmoveNumber]

fenSquares :: [[Square]]
fenSquares = map fenRow $ reverse rowNames
  where
    fenRow row = map (\c -> (c, row)) colNames

position :: Board -> T.Text
position board = T.intercalate "/" $ map (rowToFEN board) fenSquares

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

nextMove :: T.Text
nextMove = "w"

castlings :: Board -> T.Text
castlings board = if T.length result == 0 then "-" else result
  where
    result = whiteKingsideCastling board <> whiteQueensideCastling board <> blackKingsideCastling board <> blackQueensideCastling board

whiteKingsideCastling :: Board -> T.Text
whiteKingsideCastling board = castling board (Piece King White)

whiteQueensideCastling :: Board -> T.Text
whiteQueensideCastling board = castling board (Piece Queen White)

blackKingsideCastling :: Board -> T.Text
blackKingsideCastling board = castling board (Piece King Black)

blackQueensideCastling :: Board -> T.Text
blackQueensideCastling board = castling board (Piece Queen Black)

castling :: Board -> Piece -> T.Text
castling board piece@(Piece side color) = case findPiece board (col, row) of
  Just (Piece Rook color) -> case findPiece board ('e', row) of
                               Just (Piece King color) -> toFEN piece
                               Nothing                 -> ""
  _                       -> ""
  where
    row = if color == White then 1 else 8
    col = if side == King then 'h' else 'a'

enPassantTargetSquare :: T.Text
enPassantTargetSquare = "-"

halfmoveClock :: T.Text
halfmoveClock = "0"

fullmoveNumber :: T.Text
fullmoveNumber = "1"


importFEN :: T.Text -> Board
importFEN text = undefined
