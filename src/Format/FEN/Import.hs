module Format.FEN.Import where

import           Data.Char (chr, ord, digitToInt, intToDigit, isDigit)
import           Data.List.Split (splitOn)
import qualified Data.Map as M

import           Board

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
  , moveNumber = read fullmoves
  , history = []
  , fens = M.empty
  , result = Nothing
  }
  where
    [squares, color, castlings, enPassant, halfmoves, fullmoves] = words fen

parseSquares :: String -> [Maybe Piece]
parseSquares squares = concatMap parseRow $ reverse (splitOn "/" squares)

parseRow :: String -> [Maybe Piece]
parseRow "" = []
parseRow (c:cs) | isDigit c = replicate (digitToInt c) Nothing ++ parseRow cs
                | otherwise = Just (parsePiece c) : parseRow cs

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

parseEnPassangeTarget :: String -> Maybe (Square, Square)
parseEnPassangeTarget "-" = Nothing
parseEnPassangeTarget s = Just $ (target, to target)
  where
    target = read s :: Square
    to (Square s) = Square $ if s > 24 then s - 8 else s + 8

whiteKingsideCastling, whiteQueensideCastling, blackKingsideCastling, blackQueensideCastling :: String -> Bool
whiteKingsideCastling = elem 'K'
whiteQueensideCastling = elem 'Q'
blackKingsideCastling = elem 'k'
blackQueensideCastling = elem 'q'
