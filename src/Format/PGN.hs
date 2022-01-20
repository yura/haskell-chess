{-# LANGUAGE BlockArguments #-}

module Format.PGN where

import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)

import Board

--importGame :: T.Text -> Game
--importGame string = undefined

data Ply
  -- рокировка в сторону ферзя
  = PGNQueensideCastling
  -- рокировка в сторону короля
  | PGNKingsideCastling
  -- обычнй ход
  | Ply             PieceType (Maybe Char) (Maybe Int) Square
  -- взятие
  | PGNCapture          PieceType (Maybe Char) (Maybe Int) Square
  -- превращение пешки
  | PGNPromotion                                           Square PieceType
  -- взятие с последующим превращением пешки
  | PGNCapturePromotion           Char                     Square PieceType
  deriving (Eq, Show)

data MoveResult
  = Check
  | Checkmate
  deriving (Eq, Show)

data MoveAnnotated
  = MoveAnnotated Ply (Maybe MoveResult) (Maybe String)
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

parsePGNQueensideCastling :: Parser Ply
parsePGNQueensideCastling = do
  string "O-O-O"
  return PGNQueensideCastling

parsePGNKingsideCastling :: Parser Ply
parsePGNKingsideCastling = do
  string "O-O"
  return PGNKingsideCastling

-- exf8=Q
parsePGNCapturePromotion :: Parser Ply
parsePGNCapturePromotion = do
  srcCol    <- oneOf cols
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr
  string "="
  pieceName <- oneOf pieceNames

  return $ PGNCapturePromotion srcCol (col, digitToInt row) (pieceType pieceName)

-- f8=Q
parsePGNPromotion :: Parser Ply
parsePGNPromotion = do
  col       <- oneOf cols
  row       <- oneOf rowStr
  string "="
  pieceName <- oneOf pieceNames

  return $ PGNPromotion (col, digitToInt row) (pieceType pieceName)

parsePGNPawnCapture :: Parser Ply
parsePGNPawnCapture = do
  srcCol    <- oneOf cols
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr

  return $ PGNCapture Pawn (Just srcCol) Nothing (col, digitToInt row)

parsePGNCapture :: Parser Ply
parsePGNCapture = do
  pieceName <- oneOf pieceNames
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr

  return $ PGNCapture (pieceType pieceName) Nothing Nothing (col, digitToInt row)

parsePGNRegularWithSrcSquare :: Parser Ply
parsePGNRegularWithSrcSquare = do
  pieceName <- oneOf pieceNames
  srcCol    <- oneOf cols
  srcRow    <- oneOf rowStr
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ Ply (pieceType pieceName) (Just srcCol) (Just $ digitToInt srcRow) (col, digitToInt row)

parsePGNRegularWithSrcCol :: Parser Ply
parsePGNRegularWithSrcCol = do
  pieceName <- oneOf pieceNames
  srcCol    <- oneOf cols
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ Ply (pieceType pieceName) (Just srcCol) Nothing (col, digitToInt row)

parsePGNRegular :: Parser Ply
parsePGNRegular = do
  pieceName <- option 'P' $ oneOf pieceNames
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ Ply (pieceType pieceName) Nothing Nothing (col, digitToInt row)

parsePly :: Parser Ply
parsePly = do
      try parsePGNQueensideCastling
  <|> try parsePGNKingsideCastling
  <|> try parsePGNCapturePromotion
  <|> try parsePGNPromotion
  <|> try parsePGNCapture
  <|> try parsePGNPawnCapture
  <|> try parsePGNCapture
  <|> try parsePGNRegularWithSrcSquare
  <|> try parsePGNRegularWithSrcCol
  <|> parsePGNRegular

parseMoveAnnotated :: Parser MoveAnnotated
parseMoveAnnotated = do
  move <- parsePly

  mr <- optionMaybe $ try (string "+") <|> try (string "#")
  let moveResult = case mr of
        Just "+" -> Just Check
        Just "#" -> Just Checkmate
        Nothing  -> Nothing

  annotation <- optionMaybe $ try (string "??") <|> try (string "?!") <|> try (string "?")

  return $ MoveAnnotated move moveResult annotation

parseResult :: Parser Result
parseResult = do
  r <- try (string "1-0") <|> try (string "0-1") <|> string "1/2-1/2"
  return $ case r of
    "1-0"     -> WhiteWon
    "0-1"     -> BlackWon
    "1/2-1/2" -> Draw



pieceType :: Char -> PieceType
pieceType 'K' = King
pieceType 'Q' = Queen
pieceType 'R' = Rook
pieceType 'B' = Bishop
pieceType 'N' = Knight
pieceType _   = Pawn

