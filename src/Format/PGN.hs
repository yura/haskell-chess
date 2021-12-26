{-# LANGUAGE BlockArguments #-}

module Format.PGN where

import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)

import Board

--importGame :: T.Text -> Game
--importGame string = undefined

data PGNMove
  -- рокировка в сторону ферзя
  = PGNQueensideCastling
  -- рокировка в сторону короля
  | PGNKingsideCastling
  -- обычнй ход
  | PGNMove             PieceType (Maybe Char) (Maybe Int) Square
  -- взятие
  | PGNCapture          PieceType (Maybe Char) (Maybe Int) Square
  -- превращение пешки
  | PGNPromotion                                           Square PieceType
  -- взятие с последующим превращением пешки
  | PGNCapturePromotion           Char                     Square PieceType
  deriving (Eq, Show)

data Move
  = Move             Int Piece Square
  | KingsideCastling Int PieceColor
  | QueensideCastling Int PieceColor
  | Capture          Int Piece Square
  | Promotion        Int       Square Square Piece 
  | CapturePromotion Int       Square Square Piece -- взятие с превращением пешки axa1=Q
  deriving (Eq, Show)

spaces :: Parser ()
spaces = skipMany1 space

pieceNames = "KQRBN"
rowStr = concatMap show rows

parseMove = do
  number    <- many1 digit
  dots      <- try (string "...") <|> string "."
  spaces
  pieceName <- option 'P' $ oneOf pieceNames

  col       <- oneOf cols
  row       <- oneOf rowStr

  let color = case dots of
        "..." -> Black
        _     -> White
  return $ Move (read number) (Piece (pieceType pieceName) color) (col, digitToInt row) 

parsePGNQueensideCastling :: Parser PGNMove
parsePGNQueensideCastling = do
  string "O-O-O"
  return PGNQueensideCastling

parsePGNKingsideCastling :: Parser PGNMove
parsePGNKingsideCastling = do
  string "O-O"
  return PGNKingsideCastling

parsePGNPawnCapture :: Parser PGNMove
parsePGNPawnCapture = do
  srcCol    <- oneOf cols
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr

  return $ PGNCapture Pawn (Just srcCol) Nothing (col, digitToInt row)

parsePGNCapture :: Parser PGNMove
parsePGNCapture = do
  pieceName <- oneOf pieceNames
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr

  return $ PGNCapture (pieceType pieceName) Nothing Nothing (col, digitToInt row)

parsePGNRegularWithSrcSquare :: Parser PGNMove
parsePGNRegularWithSrcSquare = do
  pieceName <- oneOf pieceNames
  srcCol    <- oneOf cols
  srcRow    <- oneOf rowStr
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ PGNMove (pieceType pieceName) (Just srcCol) (Just $ digitToInt srcRow) (col, digitToInt row)

parsePGNRegularWithSrcCol :: Parser PGNMove
parsePGNRegularWithSrcCol = do
  pieceName <- oneOf pieceNames
  srcCol    <- oneOf cols
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ PGNMove (pieceType pieceName) (Just srcCol) Nothing (col, digitToInt row)

parsePGNRegular :: Parser PGNMove
parsePGNRegular = do
  pieceName <- option 'P' $ oneOf pieceNames
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ PGNMove (pieceType pieceName) Nothing Nothing (col, digitToInt row)

parsePGNMove :: Parser PGNMove
parsePGNMove = do
      try parsePGNQueensideCastling
  <|> try parsePGNKingsideCastling
  <|> try parsePGNPawnCapture
  <|> try parsePGNCapture
  <|> try parsePGNRegularWithSrcSquare
  <|> try parsePGNRegularWithSrcCol
  <|> parsePGNRegular

pieceType :: Char -> PieceType
pieceType 'K' = King
pieceType 'Q' = Queen
pieceType 'R' = Rook
pieceType 'B' = Bishop
pieceType 'N' = Knight
pieceType _   = Pawn

