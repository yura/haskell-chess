module Board.InitialPosition where

import           Board
import qualified Data.Map as M
import           Format.FEN (whiteQueensideCastling)

initialBoard :: Board
initialBoard
  = Board
  { squares = squaresAndPieces
  , nextMove = White
  , enPassantTarget = Nothing
  , whiteCanCastleKingside = True
  , whiteCanCastleQueenside = True
  , blackCanCastleKingside = True
  , blackCanCastleQueenside = True
  , halfmoveClock = 0
  , moveNumber = 1
  , history = []
  , fens = M.empty
  , result = Nothing
  }

squaresAndPieces :: [Maybe Piece]
squaresAndPieces = concat [ whitePieces, whitePawns, blanks, blackPawns, blackPieces]

whitePieces :: [Maybe Piece]
whitePieces = map Just [ rookWhite, knightWhite, bishopWhite, queenWhite, kingWhite, bishopWhite, knightWhite, rookWhite ]

whitePawns :: [Maybe Piece]
whitePawns = replicate 8 (Just pawnWhite)

blackPieces :: [Maybe Piece]
blackPieces = map Just [ rookBlack, knightBlack, bishopBlack, queenBlack, kingBlack, bishopBlack, knightBlack, rookBlack ]

blackPawns :: [Maybe Piece]
blackPawns = replicate 8 (Just pawnBlack)

blanks :: [Maybe Piece]
blanks = replicate 32 Nothing
