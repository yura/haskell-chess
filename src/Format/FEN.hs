module Format.FEN where

import           Data.Char (isDigit, digitToInt)
import           Data.List (intercalate)
import {-# SOURCE #-} Board
import           Display (squaresDisplayOrder)
import           Format.PGN.Export (squareToPGN)

-- |Экспортирует доску в формат FEN
toFEN :: Board -> String
toFEN board =  unwords
  [ exportToFENWithoutMoveNumbers board
  , show $ halfmoveClock board
  , fullmoveNumber board
  ]

exportToFENWithoutMoveNumbers :: Board -> String
exportToFENWithoutMoveNumbers board = unwords
  [ position board
  , nextMoveFEN board
  , castlings board
  , enPassantTargetSquare board
  ]

position :: Board -> String
position board = intercalate "/" $ map (rowToFEN board) squaresFENOrder

-- |Имена полей на доске, упорядоченных для экспорт в FEN
squaresFENOrder :: [[Square]]
squaresFENOrder = squaresDisplayOrder

rowToFEN :: Board -> [Square] -> String
rowToFEN board row = if counter == 0 then result else result <> show counter 
  where
  (result, counter) = helper
  helper = foldl (\(r, c) s -> case findPiece board s of
    Nothing    -> (r, c + 1)
    Just piece -> (r <> (if c > 0 then show c else "") <> pieceFEN piece, 0)) ("", 0) row

pieceFEN :: Piece -> String
pieceFEN (Piece King White)   = "K"
pieceFEN (Piece Queen White)  = "Q"
pieceFEN (Piece Rook White)   = "R"
pieceFEN (Piece Bishop White) = "B"
pieceFEN (Piece Knight White) = "N"
pieceFEN (Piece Pawn White)   = "P"
pieceFEN (Piece King Black)   = "k"
pieceFEN (Piece Queen Black)  = "q"
pieceFEN (Piece Rook Black)   = "r"
pieceFEN (Piece Bishop Black) = "b"
pieceFEN (Piece Knight Black) = "n"
pieceFEN (Piece Pawn Black)   = "p"

nextMoveFEN :: Board -> String
nextMoveFEN Board{ nextMove = White } = "w"
nextMoveFEN Board{ nextMove = Black } = "b"

castlings :: Board -> String
castlings board = if null result then "-" else result
  where
    result = whiteKingsideCastling board <> whiteQueensideCastling board <> blackKingsideCastling board <> blackQueensideCastling board

whiteKingsideCastling :: Board -> String
whiteKingsideCastling board = if whiteCanCastleKingside board then "K" else ""

whiteQueensideCastling :: Board -> String
whiteQueensideCastling board = if whiteCanCastleKingside board then "Q" else ""

blackKingsideCastling :: Board -> String
blackKingsideCastling board = if blackCanCastleKingside board then "k" else ""

blackQueensideCastling :: Board -> String
blackQueensideCastling board = if blackCanCastleKingside board then "q" else ""

enPassantTargetSquare :: Board -> String
enPassantTargetSquare board = case enPassantTarget board of
  Just s -> squareToPGN s
  _      -> "-"

fullmoveNumber :: Board -> String
fullmoveNumber Board{..} = show moveNumber
