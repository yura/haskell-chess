module Bot.Greedy where

import Board
import Laws
import Bot.Random

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  let moves = possibleMoves nextMove board
  let maxMove = maximum moves
  random <- randomMove moves

  return $ case maxMove of
      Move {} -> move board random
      _       -> move board maxMove

instance Ord Move where
  compare (CapturePromotion _ _ (Piece Queen _)) (CapturePromotion _ _ (Piece Queen _)) = EQ
  compare (CapturePromotion _ _ (Piece Queen _)) _                                      = GT
  compare _                                      (CapturePromotion _ _ (Piece Queen _)) = LT

  compare CapturePromotion {}                    CapturePromotion {}                    = EQ
  compare CapturePromotion {}                    _                                      = GT
  compare _                                      CapturePromotion {}                    = LT

  compare (Promotion _ _ (Piece Queen _))        (Promotion _ _ (Piece Queen _))        = EQ
  compare (Promotion _ _ (Piece Queen _)) _                                             = GT
  compare _                                      (Promotion _ _ (Piece Queen _))        = LT

  compare Promotion {}                           Promotion {}                           = EQ
  compare Promotion {}                             _                                    = GT
  compare _                                      Promotion {}                           = LT

  compare EnPassantCapture {}                    EnPassantCapture {}                    = EQ
  compare EnPassantCapture {}                    _                                      = GT
  compare _                                      EnPassantCapture {}                    = LT

  compare Capture {}                             Capture {}                             = EQ
  compare Capture {}                             _                                      = GT
  compare _                                      Capture {}                             = LT

  compare KingsideCastling {}                    KingsideCastling {}                    = EQ
  compare KingsideCastling {}                    _                                      = GT
  compare _                                      KingsideCastling {}                    = LT

  compare QueensideCastling {}                   QueensideCastling {}                   = EQ
  compare QueensideCastling {}                   _                                      = GT
  compare _                                      QueensideCastling {}                   = LT

  compare _                                      _                                      = EQ

