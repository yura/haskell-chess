module Board.InitialPosition where

import Board
import qualified Data.Map as M
import Format.FEN (whiteQueensideCastling)

initialBoard :: Board
initialBoard
  = Board
  { squares = M.fromList squaresAndPieces
  , enPassantTarget = Nothing
  , whiteCanCastleKingside = True
  , whiteCanCastleQueenside = True
  , blackCanCastleKingside = True
  , blackCanCastleQueenside = True
  , halfmoveClock = 0
  }

squaresAndPieces :: [(Square, Piece)]
squaresAndPieces = whitePieces ++ blackPieces

whitePieces :: [(Square, Piece)]
whitePieces = [whiteKing] ++ [whiteQueen] ++ whiteBishops ++ whiteKnights ++ whiteRooks ++ whitePawns

blackPieces :: [(Square, Piece)]
blackPieces = [blackKing] ++ [blackQueen] ++ blackBishops ++ blackKnights ++ blackRooks ++ blackPawns

pieceCols :: PieceType -> [Char]
pieceCols Pawn   = cols
pieceCols Rook   = ['a', 'h']
pieceCols Knight = ['b', 'g']
pieceCols Bishop = ['c', 'f']
pieceCols Queen  = ['d']
pieceCols King   = ['e']

pieceRow :: PieceType -> Color -> Int
pieceRow Pawn White = 2
pieceRow Pawn _     = 7
pieceRow _    White = 1
pieceRow _    _     = 8

whitePawns :: [(Square, Piece)]
whitePawns = initialSquaresFor Pawn White

blackPawns :: [(Square, Piece)]
blackPawns = initialSquaresFor Pawn Black

whiteRooks :: [(Square, Piece)]
whiteRooks = initialSquaresFor Rook White

blackRooks :: [(Square, Piece)]
blackRooks = initialSquaresFor Rook Black

whiteKnights :: [(Square, Piece)]
whiteKnights = initialSquaresFor Knight White

blackKnights :: [(Square, Piece)]
blackKnights = initialSquaresFor Knight Black

whiteBishops :: [(Square, Piece)]
whiteBishops = initialSquaresFor Bishop White

blackBishops :: [(Square, Piece)]
blackBishops = initialSquaresFor Bishop Black

whiteQueen :: (Square, Piece)
whiteQueen = head $ initialSquaresFor Queen White

blackQueen :: (Square, Piece)
blackQueen = head $ initialSquaresFor Queen Black

whiteKing :: (Square, Piece)
whiteKing = head $ initialSquaresFor King White

blackKing :: (Square, Piece)
blackKing = head $ initialSquaresFor King Black

initialSquaresFor :: PieceType -> Color -> [(Square, Piece)]
initialSquaresFor pieceType pieceColor = [((colName, row), piece) | colName <- pieceCols pieceType ]
  where
    row = pieceRow pieceType pieceColor
    piece = Piece pieceType pieceColor

