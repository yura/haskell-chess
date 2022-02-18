module Format.PGN.Export where

import Board

exportSquareToPGN :: Square -> String
exportSquareToPGN (col, row) = col : show row

exportPieceToPGN :: Piece -> String
exportPieceToPGN (Piece King _)   = "K"
exportPieceToPGN (Piece Queen _)  = "Q"
exportPieceToPGN (Piece Rook _)   = "R"
exportPieceToPGN (Piece Bishop _) = "B"
exportPieceToPGN (Piece Knight _) = "N"
exportPieceToPGN _                    = ""

exportMoveToPGN :: Move -> String
exportMoveToPGN (QueensideCastling _)                      = "O-O-O"
exportMoveToPGN (KingsideCastling _)                       = "O-O"
exportMoveToPGN (Move piece from to)                       = exportPieceToPGN piece ++ exportSquareToPGN from  ++ exportSquareToPGN to
exportMoveToPGN (EnPassantCapture piece from to)           = exportSquareToPGN from ++ "x" ++ exportSquareToPGN to
exportMoveToPGN (Capture piece@(Piece Pawn _) from to) = exportSquareToPGN from ++ "x" ++ exportSquareToPGN to
exportMoveToPGN (Capture piece from to)                    = exportPieceToPGN piece ++ exportSquareToPGN from ++ "x" ++ exportSquareToPGN to
exportMoveToPGN (Promotion from to piece)                  = exportSquareToPGN from ++ exportSquareToPGN to ++ "=" ++ exportPieceToPGN piece
exportMoveToPGN (CapturePromotion from to piece)           = exportSquareToPGN from ++ "x" ++ exportSquareToPGN to ++ "=" ++ exportPieceToPGN piece

exportMovesToPGN :: [Move] -> String
exportMovesToPGN moves = unwords $ export 1 moves
  where
    export :: Int -> [Move] -> [String]
    export moveNo []       = []
    export moveNo [w]      = (show moveNo ++ ".") : [exportMoveToPGN w]
    export moveNo (w:b:ms) = (show moveNo ++ ".") : exportMoveToPGN w : exportMoveToPGN b : export (succ moveNo) ms
