{-# LANGUAGE BlockArguments #-}

module Format.PGN.Import where

import Debug.Trace

import           Data.Char ( digitToInt, ord )
import           Data.Maybe ( fromJust, isJust )
import           Text.ParserCombinators.Parsec hiding (spaces)

import           Board hiding (Move(..), Square(..))
import qualified Board as B (Move(..), Square(..))
import           Board.InitialPosition ( initialBoard )
import           Laws

toBoard :: String -> Board
toBoard pgn = result
  where
    game = parse parseGame "PGN" pgn
    result = case game of
      Right (Game moves _) -> foldl (\b m -> move b (convertToMove m b)) initialBoard moves
      Left a               -> error $ "Ошибка чтения файла PGN" <> show a

type Square = (Char, Int)

toSquare :: Square -> B.Square
toSquare (c, r) = B.Square $ (r - 1) * 8 + (ord c - 97)

-- Доска нужна для решения неоднозначных ситуаций, когда поле откуда ходила фигура не задано.
-- В этом случае необходимо искать, с какого поля был сделан ход.
convertToMove :: Move -> Board -> B.Move
convertToMove m b
  = case m of
      (Move _ color (PlyAnnotated KingsideCastling _ _)) ->
        B.KingsideCastling color
      (Move _ color (PlyAnnotated QueensideCastling _ _)) ->
        B.QueensideCastling color
      (Move _ color (PlyAnnotated (Ply pt (Just fromCol) (Just fromRow) to) _ _)) ->
        B.Move (Piece pt color) (toSquare (fromCol, fromRow)) (toSquare to)
      (Move _ color (PlyAnnotated (Ply pt (Just file) Nothing to) _ _)) ->
        B.Move (Piece pt color) (findValidFromSquareByFromFileAndTo (Piece pt color) file (toSquare to) b) (toSquare to)
      (Move _ color (PlyAnnotated (Ply pt Nothing Nothing to) _ _)) ->
        B.Move (Piece pt color) (findValidFromSquare (Piece pt color) (toSquare to) b) (toSquare to)
      (Move _ color (PlyAnnotated (Capture pt Nothing Nothing to) _ _)) ->
        B.Capture (Piece pt color) (findValidFromSquare (Piece pt color) (toSquare to) b) (toSquare to)
      (Move _ color (PlyAnnotated (Capture pt (Just file) Nothing to) _ _)) ->
        B.Capture (Piece pt color) (findValidFromSquareByFromFileAndTo (Piece pt color) file (toSquare to) b) (toSquare to)
      (Move _ color (PlyAnnotated (Promotion to pt) _ _)) ->
        B.Promotion (findValidFromSquare (Piece pt color) (toSquare to) b) (toSquare to) (Piece pt color)
      (Move _ color (PlyAnnotated (CapturePromotion file to pt) _ _)) ->
        B.CapturePromotion (findValidFromSquare (Piece pt color) (toSquare to) b) (toSquare to) (Piece pt color)
      _ -> error $ "Странный ход: " <> show m

-- Поиск возможного исходного поля для фигуры
findValidFromSquare :: Piece -> B.Square -> Board -> B.Square
findValidFromSquare piece to board = head $ findAllValidFromSquare piece to board

findValidFromSquareByFromFileAndTo :: Piece -> Char -> B.Square -> Board -> B.Square
findValidFromSquareByFromFileAndTo piece file to board
  = head $ filterByFile file $ findAllValidFromSquare piece to board

findAllValidFromSquare :: Piece -> B.Square -> Board -> [B.Square]
findAllValidFromSquare piece to board
  = map fromJust
  $ filter isJust
  $ map (fromSquareByPieceTo piece to) $ possibleMoves board 
  where
    fromSquareByPieceTo :: Piece -> B.Square -> B.Move -> Maybe B.Square
    fromSquareByPieceTo piece to (B.Move p f t) = if piece == p && to == t then Just f else Nothing
    fromSquareByPieceTo piece to (B.Move p f t) = if piece == p && to == t then Just f else Nothing
    fromSquareByPieceTo piece to (B.Capture p f t) = if piece == p && to == t then Just f else Nothing
    fromSquareByPieceTo piece to (B.EnPassantCapture p f t) = if piece == p && to == t then Just f else Nothing
-- не знаю что для них?
--  | Promotion               Square Square Piece
--  | CapturePromotion        Square Square Piece
    fromSquareByPieceTo _ _ _ = Nothing

filterByFile :: Char -> [B.Square] -> [B.Square]
filterByFile file = filter (\s -> squareFile s == toCol file)

toCol :: Char -> Int
toCol file = (ord file - 97) `mod` 8

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

data PlyResult
  = Check
  | Checkmate
  deriving (Eq, Show)

data PlyAnnotated
  = PlyAnnotated Ply (Maybe PlyResult) (Maybe String)
  deriving (Eq, Show)

data Move
  = Move Int Color PlyAnnotated
  deriving (Eq, Show)

data Game
  = Game [Move] Result
  deriving (Eq, Show)

spaces :: Parser ()
spaces = skipMany1 space

pieceNames = "KQRBN"
rowStr = concatMap show rows

parseGame :: Parser Game
parseGame = do
  firstMove <- parseMove
  moves     <- manyTill (spaces >> parseMove) (lookAhead parseResult)
  --spaces
  result    <- parseResult
  --optional spaces

  optional $ string "\n"

  eof

  return $ Game (concat $ firstMove:moves) result

parseMove :: Parser [Move]
parseMove = do
  number  <- many1 digit
  dots    <- try (string "...") <|> string "."
  optional spaces
  ply     <- parsePlyAnnotated
  ply'    <- optionMaybe $ try (spaces >> parsePlyAnnotated)

  let color = case dots of
        "..." -> Black
        _     -> White

  return $ case ply' of
    Just p -> [Move (read number) color ply, Move (read number) Black p]
    _      -> [Move (read number) color ply]

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
  ply <- parsePly

  mr <- optionMaybe $ try (string "+") <|> try (string "#")
  let moveResult = case mr of
        Just "+" -> Just Check
        Just "#" -> Just Checkmate
        Nothing  -> Nothing

  annotation <- optionMaybe $ try (string "??") <|> try (string "?!") <|> try (string "?")

  return $ PlyAnnotated ply moveResult annotation

pieceType :: Char -> PieceType
pieceType 'K' = King
pieceType 'Q' = Queen
pieceType 'R' = Rook
pieceType 'B' = Bishop
pieceType 'N' = Knight
pieceType _   = Pawn

parseResult :: Parser Result
parseResult = do
  r <- try (string " 1-0") <|> try (string " 0-1") <|> try (string " 1/2-1/2")
  return $ case r of
    " 1-0"     -> WhiteWon
    " 0-1"     -> BlackWon
    " 1/2-1/2" -> Draw NotDefined
