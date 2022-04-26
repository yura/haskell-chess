module Bot.Human where

import           Control.Concurrent (threadDelay)

import Board
import Display
import qualified Format.PGN.Import as PGN
import Laws
import GHC.Base (RuntimeRep(Int16Rep))
import Data.Char (digitToInt)

parseMove :: Board -> String -> Maybe Move

parseMove Board{..} "O-O-O" = Just $ QueensideCastling nextMove
parseMove Board{..} "O-O" = Just $ KingsideCastling nextMove
parseMove Board{..} [fromCol, fromRow, 'x', toCol, toRow, '=', piece] = Just $ CapturePromotion (fromCol, digitToInt fromRow) (toCol, digitToInt toRow) (Piece (PGN.pieceType piece) nextMove)
parseMove Board{..} [fromCol, fromRow, toCol, toRow, '=', piece] = Just $ Promotion (fromCol, digitToInt fromRow) (toCol, digitToInt toRow) (Piece (PGN.pieceType piece) nextMove)
parseMove Board{..} [piece, fromCol, fromRow, 'x', toCol, toRow] = Just $ Capture (Piece (PGN.pieceType piece) nextMove) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove Board{..} [fromCol, fromRow, 'x', toCol, toRow] = Just $ Capture (Piece Pawn nextMove) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove Board{..} [piece, fromCol, fromRow, toCol, toRow] = Just $ Move (Piece (PGN.pieceType piece) nextMove) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
parseMove Board{..} [fromCol, fromRow, toCol, toRow] = Just $ Move (Piece Pawn nextMove) (fromCol, digitToInt fromRow) (toCol, digitToInt toRow)
--  | EnPassantCapture  Piece Square Square
parseMove _ _ = Nothing

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  renderBoard board
  putStr "Type your move: "
  moveStr <- getLine

  case parseMove board moveStr of
    Nothing -> do
      putStrLn "Wrong move notation, try again"
      threadDelay 500000
      makeMove board
    Just m  -> do
      let validMoves = possibleMoves board
      if m `elem` validMoves
        then return $ move board m
        else do
          putStrLn "Invalid move, try again"
          threadDelay 500000
          makeMove board          
