{-# LANGUAGE TupleSections #-}

module Display where

import           Data.Char (chr)
import qualified Data.Text as T
import {-# SOURCE #-} Board (Board(..), Piece(..), PieceType(..), Color(..), Square, cols, rows, findPiece)

clearScreen :: IO ()
clearScreen = do
  putStrLn $ chr 27 : "[2J"
  putStrLn $ chr 27 : "[;H"

renderBoard :: Board -> IO ()
renderBoard board = do
  clearScreen
  putStrLn $ T.unpack $ exportToDisplay board True

exportToDisplay :: Board -> Bool -> T.Text
exportToDisplay board renderNames = boardText <> if renderNames then colNames else ""
  where
    boardText = T.intercalate "\n" rowsListWithRowNames
    rowsListWithRowNames = if renderNames
      then zipWith (<>) rowNames rowsList
      else rowsList
    rowNames = map (\i -> T.pack $ show i <> " ") $ reverse rows
    rowsList = map (rowToDisplay board) squaresDisplayOrder
    colNames :: T.Text
    colNames = "\n\n  " <> T.pack cols

rowToDisplay :: Board -> [Square] -> T.Text
rowToDisplay board row = T.concat $ map (maybe " " pieceToDisplay . findPiece board) row

-- |Имена полей в порядке, который удобен для вывода доски на экран
squaresDisplayOrder :: [[Square]]
squaresDisplayOrder = map colName $ reverse rows
  where
    colName row = map (, row) cols

pieceToDisplay :: Piece -> T.Text
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