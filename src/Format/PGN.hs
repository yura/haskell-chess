{-# LANGUAGE BlockArguments #-}

module Format.PGN where

import Data.Char (digitToInt)
import Text.ParserCombinators.Parsec hiding (spaces)

import Board

--importGame :: T.Text -> Game
--importGame string = undefined

data Ply
  -- рокировка в сторону ферзя
  = QueensideCastling
  -- рокировка в сторону короля
  | KingsideCastling
  -- обычнй ход
  | Ply              PieceType (Maybe Char) (Maybe Int) Square
  -- взятие
  | Capture          PieceType (Maybe Char) (Maybe Int) Square
  -- превращение пешки
  | Promotion                                           Square PieceType
  -- взятие с последующим превращением пешки
  | CapturePromotion           Char                     Square PieceType
  deriving (Eq, Show)

data Move
  = Move Int Color PlyAnnotated
  deriving (Eq, Show)

data MoveResult
  = Check
  | Checkmate
  deriving (Eq, Show)

data PlyAnnotated
  = PlyAnnotated Ply (Maybe MoveResult) (Maybe String)
  deriving (Eq, Show)

spaces :: Parser ()
spaces = skipMany1 space

pieceNames = "KQRBN"
rowStr = concatMap show rows

{-
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
-}

parseQueensideCastling :: Parser Ply
parseQueensideCastling = do
  string "O-O-O"
  return QueensideCastling

parseKingsideCastling :: Parser Ply
parseKingsideCastling = do
  string "O-O"
  return KingsideCastling

-- exf8=Q
parseCapturePromotion :: Parser Ply
parseCapturePromotion = do
  srcCol    <- oneOf cols
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr
  string "="
  pieceName <- oneOf pieceNames

  return $ CapturePromotion srcCol (col, digitToInt row) (pieceType pieceName)

-- f8=Q
parsePromotion :: Parser Ply
parsePromotion = do
  col       <- oneOf cols
  row       <- oneOf rowStr
  string "="
  pieceName <- oneOf pieceNames

  return $ Promotion (col, digitToInt row) (pieceType pieceName)

parsePawnCapture :: Parser Ply
parsePawnCapture = do
  srcCol    <- oneOf cols
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr

  return $ Capture Pawn (Just srcCol) Nothing (col, digitToInt row)

parseCapture :: Parser Ply
parseCapture = do
  pieceName <- oneOf pieceNames
  string "x"
  col       <- oneOf cols
  row       <- oneOf rowStr

  return $ Capture (pieceType pieceName) Nothing Nothing (col, digitToInt row)

parseRegularWithSrcSquare :: Parser Ply
parseRegularWithSrcSquare = do
  pieceName <- oneOf pieceNames
  srcCol    <- oneOf cols
  srcRow    <- oneOf rowStr
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ Ply (pieceType pieceName) (Just srcCol) (Just $ digitToInt srcRow) (col, digitToInt row)

parseRegularWithSrcCol :: Parser Ply
parseRegularWithSrcCol = do
  pieceName <- oneOf pieceNames
  srcCol    <- oneOf cols
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ Ply (pieceType pieceName) (Just srcCol) Nothing (col, digitToInt row)

parseRegular :: Parser Ply
parseRegular = do
  pieceName <- option 'P' $ oneOf pieceNames
  col       <- oneOf cols
  row       <- oneOf rowStr
  return $ Ply (pieceType pieceName) Nothing Nothing (col, digitToInt row)

parsePly :: Parser Ply
parsePly = do
      try parseQueensideCastling
  <|> try parseKingsideCastling
  <|> try parseCapturePromotion
  <|> try parsePromotion
  <|> try parseCapture
  <|> try parsePawnCapture
  <|> try parseCapture
  <|> try parseRegularWithSrcSquare
  <|> try parseRegularWithSrcCol
  <|> parseRegular

parsePlyAnnotated :: Parser PlyAnnotated
parsePlyAnnotated = do
  move <- parsePly

  mr <- optionMaybe $ try (string "+") <|> try (string "#")
  let moveResult = case mr of
        Just "+" -> Just Check
        Just "#" -> Just Checkmate
        Nothing  -> Nothing

  annotation <- optionMaybe $ try (string "??") <|> try (string "?!") <|> try (string "?")

  return $ PlyAnnotated move moveResult annotation

pieceType :: Char -> PieceType
pieceType 'K' = King
pieceType 'Q' = Queen
pieceType 'R' = Rook
pieceType 'B' = Bishop
pieceType 'N' = Knight
pieceType _   = Pawn

parseResult :: Parser Result
parseResult = do
  r <- try (string "1-0") <|> try (string "0-1") <|> string "1/2-1/2"
  return $ case r of
    "1-0"     -> WhiteWon
    "0-1"     -> BlackWon
    "1/2-1/2" -> Draw


