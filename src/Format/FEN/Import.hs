module Format.FEN.Import where

import           Data.Char (chr, ord, digitToInt, intToDigit, isDigit)
import           Data.List.Split (splitOn)
import qualified Data.Map as M
import qualified Data.Vector as V

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

parseSquares :: String -> V.Vector (Maybe Piece)
parseSquares squares = V.concat $ map (parseRow) $ reverse (splitOn "/" squares)

parseRow :: String -> V.Vector (Maybe Piece)
parseRow "" = V.empty
parseRow (c:cs) | isDigit c = V.replicate (digitToInt c) Nothing V.++ parseRow cs
                | otherwise = Just (parsePiece c) `V.cons` parseRow cs

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