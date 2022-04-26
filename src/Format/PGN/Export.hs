module Format.PGN.Export where

import {-# SOURCE #-} Board

squareToPGN :: Square -> String
squareToPGN (col, row) = col : show row

pieceToPGN :: Piece -> String
pieceToPGN (Piece King _)   = "K"
pieceToPGN (Piece Queen _)  = "Q"
pieceToPGN (Piece Rook _)   = "R"
pieceToPGN (Piece Bishop _) = "B"
pieceToPGN (Piece Knight _) = "N"
pieceToPGN _                = ""

moveToPGN :: Move -> String
moveToPGN (QueensideCastling _)            = "O-O-O"
moveToPGN (KingsideCastling _)             = "O-O"
moveToPGN (Move piece from to)             = pieceToPGN piece ++ squareToPGN from  ++ squareToPGN to
moveToPGN (EnPassantCapture piece from to) = squareToPGN from ++ "x" ++ squareToPGN to
moveToPGN (Capture piece from to)          = pieceToPGN piece ++ squareToPGN from ++ "x" ++ squareToPGN to
moveToPGN (Promotion from to piece)        = squareToPGN from ++ squareToPGN to ++ "=" ++ pieceToPGN piece
moveToPGN (CapturePromotion from to piece) = squareToPGN from ++ "x" ++ squareToPGN to ++ "=" ++ pieceToPGN piece

movesToPGN :: [Move] -> String
movesToPGN moves = unwords $ export 1 moves
  where
    export :: Int -> [Move] -> [String]
    export moveNo []       = []
    export moveNo [w]      = (show moveNo ++ ".") : [moveToPGN w]
    export moveNo (w:b:ms) = (show moveNo ++ ".") : moveToPGN w : moveToPGN b : export (succ moveNo) ms

toPGN :: Board -> String
toPGN = movesToPGN . reverse . history