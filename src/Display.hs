{-# LANGUAGE TupleSections #-}

module Display where

import           Data.Char ( chr )
import           Data.List ( intercalate )
import {-# SOURCE #-} Board
-- import Laws.King (piece)
import qualified Format.PGN.Export as PGN

printBoard :: Board -> IO ()
printBoard board = do
  clearDisplay
  putStrLn $ toDisplay board True
  putStrLn ""

clearDisplay :: IO ()
clearDisplay = do
  putStrLn $ chr 27 : "[2J"
  putStrLn $ chr 27 : "[;H"

toDisplay :: Board -> Bool -> String
toDisplay board displayLabels = boardText <> if displayLabels then colNames else ""
  where
    boardText = intercalate "\n" rowsListWithRowNames
    rowsListWithRowNames = if displayLabels
      then zipWith (<>) rowNames rowsList
      else rowsList
    rowNames = map (\i -> show i <> " ") $ reverse rows
    rowsList = map (rowToDisplay board) squaresDisplayOrder
    colNames :: String
    colNames = "\n\n  " <> cols

rowToDisplay :: Board -> [Square] -> String
rowToDisplay board = concatMap (maybe " " pieceToDisplay . findPiece board)

-- |Имена полей в порядке, который удобен для вывода доски на экран
squaresDisplayOrder :: [[Square]]
squaresDisplayOrder = reorderSquares [0..63] []
  where
    reorderSquares :: [Int] -> [[Square]] -> [[Square]] 
    reorderSquares [] result = result
    reorderSquares xs result = reorderSquares rest $ (map Square row) : result
      where (row, rest) = splitAt 8 xs 

pieceToDisplay :: Piece -> String
pieceToDisplay (Piece King White)   = "♔"
pieceToDisplay (Piece Queen White)  = "♕"
pieceToDisplay (Piece Rook White)   = "♖"
pieceToDisplay (Piece Bishop White) = "♗"
pieceToDisplay (Piece Knight White) = "♘"
pieceToDisplay (Piece Pawn White)   = "♙"
pieceToDisplay (Piece King Black)   = "♚"
pieceToDisplay (Piece Queen Black)  = "♛"
pieceToDisplay (Piece Rook Black)   = "♜"
pieceToDisplay (Piece Bishop Black) = "♝"
pieceToDisplay (Piece Knight Black) = "♞"
pieceToDisplay (Piece Pawn Black)   = "♟"

moveToDisplay :: Move -> String
moveToDisplay (QueensideCastling _) = "O-O-O"
moveToDisplay (KingsideCastling _) = "O-O"
moveToDisplay (Move (Piece Pawn _) from to) = PGN.squareToPGN from ++ PGN.squareToPGN to
moveToDisplay (Capture (Piece Pawn _) from to) = PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to
moveToDisplay (Move p from to) = PGN.pieceToPGN p ++ PGN.squareToPGN from ++ PGN.squareToPGN to
moveToDisplay (Capture p from to) = PGN.pieceToPGN p ++ PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to
moveToDisplay (Promotion from to p) = PGN.squareToPGN from ++ PGN.squareToPGN to ++ "=" ++ PGN.pieceToPGN p
moveToDisplay (CapturePromotion from to p) = PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to ++ "=" ++ PGN.pieceToPGN p
moveToDisplay (EnPassantCapture _ from to) = PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to


moveToDisplay' :: Move -> String
moveToDisplay' (QueensideCastling _) = "O-O-O"
moveToDisplay' (KingsideCastling _) = "O-O"
moveToDisplay' (Move (Piece Pawn _) from to) = PGN.squareToPGN from ++ PGN.squareToPGN to
moveToDisplay' (Capture (Piece Pawn _) from to) = PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to
moveToDisplay' (Move p from to) = pieceToDisplay p ++ PGN.squareToPGN from ++ PGN.squareToPGN to
moveToDisplay' (Capture p from to) = pieceToDisplay p ++ PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to
moveToDisplay' (Promotion from to p) = PGN.squareToPGN from ++ PGN.squareToPGN to ++ "=" ++ pieceToDisplay p
moveToDisplay' (CapturePromotion from to p) = PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to ++ "=" ++ pieceToDisplay p
moveToDisplay' (EnPassantCapture _ from to) = PGN.squareToPGN from ++ "x" ++ PGN.squareToPGN to
