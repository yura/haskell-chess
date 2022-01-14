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

    context "рокировки" $ do
      it "рокировка в сторону ферзя" $
        parsePGNMove' "O-O-O" `shouldParse` PGNQueensideCastling

      it "рокировка в сторону короля" $
        parsePGNMove' "O-O" `shouldParse` PGNKingsideCastling

    context "обычные ходы" $ do
      it "ход пешкой" $
        parsePGNMove' "e4" `shouldParse` PGNMove Pawn Nothing Nothing ('e', 4)

      it "ход ладьёй" $
        parsePGNMove' "Ra5" `shouldParse` PGNMove Rook Nothing Nothing ('a', 5)

      it "ход конём" $
        parsePGNMove' "Ng2" `shouldParse` PGNMove Knight Nothing Nothing ('g', 2)

      it "ход слоном" $
        parsePGNMove' "Bc7" `shouldParse` PGNMove Bishop Nothing Nothing ('c', 7)

      it "ход ферзём" $
        parsePGNMove' "Qh8" `shouldParse` PGNMove Queen Nothing Nothing ('h', 8)

      it "ход королём" $
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

    context "ходы с указанием исходной вертикали" $ do
      it "ход ладьёй" $
        parsePGNMove' "Rae5" `shouldParse` PGNMove Rook (Just 'a') Nothing ('e', 5)
     
      it "ход конём" $
        parsePGNMove' "Nbe6" `shouldParse` PGNMove Knight (Just 'b') Nothing ('e', 6)
     
      it "ход слоном" $
        parsePGNMove' "Bcf7" `shouldParse` PGNMove Bishop (Just 'c') Nothing ('f', 7)
     
      it "ход ферзём" $
        parsePGNMove' "Qdg8" `shouldParse` PGNMove Queen (Just 'd') Nothing ('g', 8)
     
      it "ход королём" $ do
        pendingWith "король должен быть один, при указании вертикали короля, скорее всего надо падать"
        parsePGNMove' "Kab2" `shouldParse` PGNMove King (Just 'a') Nothing ('b', 2)

    context "ходы с указанием исходной клетки" $ do
      it "ход ладьёй" $
        parsePGNMove' "Rd1d2" `shouldParse` PGNMove Rook (Just 'd') (Just 1) ('d', 2)
     
      it "ход конём" $
        parsePGNMove' "Nc1d3" `shouldParse` PGNMove Knight (Just 'c') (Just 1) ('d', 3)
     
      it "ход слоном" $
        parsePGNMove' "Be1f2" `shouldParse` PGNMove Bishop (Just 'e') (Just 1) ('f', 2)
     
      it "ход ферзём" $
        parsePGNMove' "Qe4e6" `shouldParse` PGNMove Queen (Just 'e') (Just 4) ('e', 6)
     
      it "ход королём" $ do
        pendingWith "король должен быть один, при указании вертикали короля, скорее всего надо падать"
        parsePGNMove' "Kab2" `shouldParse` PGNMove King (Just 'a') Nothing ('b', 2)

    context "продвижение (c8=Q)" $ do
      it "белая пешка становится ферзём" $
        parsePGNMove' "c8=Q" `shouldParse` PGNPromotion ('c', 8) Queen

      it "белая пешка становится конём" $
        parsePGNMove' "h8=N" `shouldParse` PGNPromotion ('h', 8) Knight

      it "черная пешка становится ладьёй" $
        parsePGNMove' "a1=R" `shouldParse` PGNPromotion ('a', 1) Rook

    context "взятие с продвижением (exf8=Q)" $ do
      it "белая пешка становится ферзём" $
        parsePGNMove' "exf8=Q" `shouldParse` PGNCapturePromotion 'e' ('f', 8) Queen

      it "чёрная пешка становится ферзём" $
        parsePGNMove' "hxg1=Q" `shouldParse` PGNCapturePromotion 'h' ('g', 1) Queen

  describe "parseMoveAnnotated" $ do
    let parseMoveAnnotated' = parse parseMoveAnnotated "PGN"

    it "обычный ход" $
      parseMoveAnnotated' "Bxf6" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) Nothing Nothing

    context "шах" $ do
      it "возращает шах" $
        parseMoveAnnotated' "Bxf6+" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) (Just Check) Nothing

    context "мат" $ do
      it "возращает мат" $
        parseMoveAnnotated' "Bxf6#" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) (Just Checkmate) Nothing

    context "аннотация" $ do
      it "возращает ??" $
        parseMoveAnnotated' "Bxf6??" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) Nothing (Just "??")

      it "возращает ?!" $
        parseMoveAnnotated' "Bxf6?!" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) Nothing (Just "?!")

      it "возращает ?" $
        parseMoveAnnotated' "Bxf6?" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) Nothing (Just "?")

      it "шах и аннотация ??" $
        parseMoveAnnotated' "Bxf6+??" `shouldParse` MoveAnnotated (PGNCapture Bishop Nothing Nothing ('f', 6)) (Just Check) (Just "??")

    --context "результат партии"

