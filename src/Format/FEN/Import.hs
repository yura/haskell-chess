module Format.FEN.Import where

import qualified Data.Map as M
import           Data.List.Split (splitOn)

import           Board
import Data.Char (digitToInt, intToDigit, isDigit)

fromFEN :: String -> Board
fromFEN fen
  = Board
  { squares  = parseSquares squares
  , nextMove = parseNextMove color
  , enPassantTarget = parseEnPassangeTarget enPassant
  , whiteCanCastleKingside = whiteKingsideCastling castlings
  , whiteCanCastleQueenside = whiteQueensideCastling castlings
  , blackCanCastleKingside = blackKingsideCastling castlings
  , blackCanCastleQueenside = blackQueensideCastling castlings
  , halfmoveClock = read halfmoves
  , history = []
  , fens = M.empty
  , result = Nothing
  }
  where
    [squares, color, castlings, enPassant, halfmoves, fullmoves] = words fen

parseSquares :: String -> M.Map Square Piece
parseSquares squares = M.fromList $ concat $ zipWith (`parseRow` 'a') (splitOn "/" squares) (reverse rows)

parseRow :: String -> Char -> Int -> [(Square, Piece)]
parseRow "" _ _ = []
parseRow (c:cs) col row | isDigit c = parseRow cs (intToDigit $ digitToInt c + digitToInt col) row
                        | otherwise = ((col, row), parsePiece c) : parseRow cs (succ col) row

parsePiece :: Char -> Piece 
parsePiece 'K' = kingWhite
parsePiece 'Q' = queenWhite
parsePiece 'R' = rookWhite
parsePiece 'B' = bishopWhite
parsePiece 'N' = knightWhite
parsePiece 'P' = pawnWhite
parsePiece 'k' = kingBlack
parsePiece 'q' = queenBlack
parsePiece 'r' = rookBlack
parsePiece 'b' = bishopBlack
parsePiece 'n' = knightBlack
parsePiece 'p' = pawnBlack

parseNextMove :: String -> Color 
parseNextMove color = if color == "w" then White else Black

parseEnPassangeTarget :: String -> Maybe Square 
parseEnPassangeTarget "-" = Nothing
parseEnPassangeTarget (c:r:[]) = Just (c, digitToInt r)

whiteKingsideCastling, whiteQueensideCastling, blackKingsideCastling, blackQueensideCastling :: String -> Bool
whiteKingsideCastling = elem 'K'
whiteQueensideCastling = elem 'Q'
blackKingsideCastling = elem 'k'
blackQueensideCastling = elem 'q'