module Board.InitialPosition where

import           Board
import qualified Data.Map as M
import qualified Data.Vector as V
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

squaresAndPieces :: V.Vector (Maybe Piece)
squaresAndPieces = V.concat [ whitePieces, whitePawns, blanks, blackPawns, blackPieces]

whitePieces :: V.Vector (Maybe Piece)
whitePieces = V.fromList $ map Just [ rookWhite, knightWhite, bishopWhite, queenWhite, kingWhite, bishopWhite, knightWhite, rookWhite ]

whitePawns :: V.Vector (Maybe Piece)
whitePawns = V.replicate 8 (Just pawnWhite)

blackPieces :: V.Vector (Maybe Piece)
blackPieces = V.fromList $ map Just [ rookBlack, knightBlack, bishopBlack, queenBlack, kingBlack, bishopBlack, knightBlack, rookBlack ]

blackPawns :: V.Vector (Maybe Piece)
blackPawns = V.replicate 8 (Just pawnBlack)

blanks :: V.Vector (Maybe Piece)
blanks = V.replicate 32 Nothing
