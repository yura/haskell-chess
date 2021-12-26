{-# LANGUAGE NoOverloadedStrings #-}

module Format.PGNSpec (spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

--import Game
import Board
import Format.PGN

spec :: Spec
spec = do
{-
  describe "parseMoves" $ do
    let parseMoves' = parse parseMoves "PGN"

    it "возращает белую пешку и поле" $ do
      parseMove' "1. e4 e5" `shouldParse` [Move 1 (Piece Pawn White) ('e', 4), Move 1 (Piece Pawn Black)]
-}

  describe "parseMove" $ do
    let parseMove' = parse parseMove "PGN"

    context "[ход белых и чёрных объединены под одним номером]" $ do
      context "[пешка (название фигуры пропущено)]" $ do
        it "возращает белую пешку и поле" $ do
          parseMove' "1. e4" `shouldParse` Move 1 (Piece Pawn White) ('e', 4)

        it "возращает чёрную пешку и поле" $
          parseMove' "1... e5" `shouldParse` Move 1 (Piece Pawn Black) ('e', 5)

        context "[превращение пешки]" $ do
          it "превращение пешки" $ do
            pending
            --parseMove' "6. d8=Q" `shouldParse` Promotion 6 ('d', 8) (Piece Queen White)  

        --it "взятие с превращение пешки" $
        --  parseMove' "6. d8=Q" `shouldParse` 

      context "[ладья]" $ do
        it "возращает фигуру и поле" $ do
          parseMove' "15. Ra7" `shouldParse` Move 15 (Piece Rook White) ('a', 7)

        it "возращает фигуру и поле" $
          parseMove' "17... Rh7" `shouldParse` Move 17 (Piece Rook Black) ('h', 7)

        it "взятие" $ do
          pending
          parseMove' "19. Rxc6" `shouldParse` Capture 19 (Piece Rook White) ('c', 6)

        it "шах после хода" $ pending

        it "шах после взятия" $ pending

        it "указание с какого места был ход" $ pending

        it "рокировка в короткую сторону" $ pending

        it "рокировка в длинную сторону" $ pending


      
    context "[ход белых отделён от хода чёрных]" $ do


      context "[пешка (название фигуры пропущено)]" $ do
        it "возращает белую пешку и поле" $ do
          parseMove' "1. e4" `shouldParse` Move 1 (Piece Pawn White) ('e', 4)

        it "возращает чёрную пешку и поле" $
          parseMove' "1... e5" `shouldParse` Move 1 (Piece Pawn Black) ('e', 5)

        context "[превращение пешки]" $ do
          it "превращение пешки" $ do
            pending
            --parseMove' "6. d8=Q" `shouldParse` Promotion 6 ('d', 8) (Piece Queen White)  

        --it "взятие с превращение пешки" $
        --  parseMove' "6. d8=Q" `shouldParse` 

      context "[ладья]" $ do
        it "возращает фигуру и поле" $ do
          parseMove' "15. Ra7" `shouldParse` Move 15 (Piece Rook White) ('a', 7)

        it "возращает фигуру и поле" $
          parseMove' "17... Rh7" `shouldParse` Move 17 (Piece Rook Black) ('h', 7)

        it "взятие" $ do
          pending
          parseMove' "19. Rxc6" `shouldParse` Capture 19 (Piece Rook White) ('c', 6)

        it "шах после хода" $ pending

        it "шах после взятия" $ pending

        it "указание с какого места был ход" $ pending

        it "рокировка в короткую сторону" $ pending

        it "рокировка в длинную сторону" $ pending

  describe "parsePGNMove" $ do
    let parsePGNMove' = parse parsePGNMove "PGN"

    it "рокировка в сторону ферзя" $ do
      parsePGNMove' "O-O-O" `shouldParse` PGNQueensideCastling

    it "рокировка в сторону короля" $ do
      parsePGNMove' "O-O" `shouldParse` PGNKingsideCastling

    it "ход пешкой" $ do
      parsePGNMove' "e4" `shouldParse` PGNMove Pawn Nothing Nothing ('e', 4)

    it "ход ладьёй" $ do
      parsePGNMove' "Ra5" `shouldParse` PGNMove Rook Nothing Nothing ('a', 5)

    it "ход конём" $ do
      parsePGNMove' "Ng2" `shouldParse` PGNMove Knight Nothing Nothing ('g', 2)

    it "ход слоном" $ do
      parsePGNMove' "Bc7" `shouldParse` PGNMove Bishop Nothing Nothing ('c', 7)

    it "ход ферзём" $ do
      parsePGNMove' "Qh8" `shouldParse` PGNMove Queen Nothing Nothing ('h', 8)

    it "ход королём" $ do
      parsePGNMove' "Ka4" `shouldParse` PGNMove King Nothing Nothing ('a', 4)

    context "ходы со взятием" $ do
      it "взятие пешкой" $
        parsePGNMove' "bxa5" `shouldParse` PGNCapture Pawn (Just 'b') Nothing ('a', 5)

      it "взятие ладьёй" $
        parsePGNMove' "Rxa5" `shouldParse` PGNCapture Rook Nothing Nothing ('a', 5)

      it "взятие конём" $
        parsePGNMove' "Nxg2" `shouldParse` PGNCapture Knight Nothing Nothing ('g', 2)

      it "взятие слоном" $
        parsePGNMove' "Bxc8" `shouldParse` PGNCapture Bishop Nothing Nothing ('c', 8)

      it "взятие ферзём" $
        parsePGNMove' "Qxh6" `shouldParse` PGNCapture Queen Nothing Nothing ('h', 6)

      it "взятие королём" $
        parsePGNMove' "Kxa3" `shouldParse` PGNCapture King Nothing Nothing ('a', 3)

    context "ходы с указанием вертикали" $ do
      it "ход ладьёй" $ do
        pending
        parsePGNMove' "Rae5" `shouldParse` PGNMove Rook (Just 'a') Nothing ('a', 5)
     
