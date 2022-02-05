{-# LANGUAGE NoOverloadedStrings #-}

module Format.PGNSpec (spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

--import Game
import Board hiding (Move(..))
import Format.PGN

spec :: Spec
spec = do
{-
  describe "parseMoves" $ do
    let parseMoves' = parse parseMoves "PGN"

    it "возращает белую пешку и поле" $ do
      parseMove' "1. e4 e5" `shouldParse` [Move 1 (Piece Pawn White) ('e', 4), Move 1 (Piece Pawn Black)]
-}

  describe "parseGame" $ do
    let parseGame' = parse parseGame "PGN"

    it "возращает игру со списком ходов и результатом" $
      parseGame' "1. d4 f5 2. Nc3 Qh4 0-1" `shouldParse`
        Game
          [ Move 1 White (PlyAnnotated (Ply Pawn Nothing Nothing ('d', 4)) Nothing Nothing)
          , Move 1 Black (PlyAnnotated (Ply Pawn Nothing Nothing ('f', 5)) Nothing Nothing)
          , Move 2 White (PlyAnnotated (Ply Knight Nothing Nothing ('c', 3)) Nothing Nothing)
          , Move 2 Black (PlyAnnotated (Ply Queen Nothing Nothing ('h', 4)) Nothing Nothing)
          ] BlackWon

  describe "parseResult" $ do
    let parseResult' = parse parseResult "PGN"

    it "возвращает победу белых, если результат записан в виде '1-0'" $
      parseResult' " 1-0" `shouldParse` WhiteWon

    it "возвращает победу чёрных, если результат записан в виде '0-1'" $
      parseResult' " 0-1" `shouldParse` BlackWon

    it "возвращает ничью чёрных, если результат записан в виде '1/2-1/2'" $
      parseResult' " 1/2-1/2" `shouldParse` Draw

  describe "parseMove" $ do
    let parseMove' = parse parseMove "PGN"

    context "[ход белых отделён от хода чёрных]" $ do
        it "возращает белую пешку и поле" $ do
          parseMove' "1. e4" `shouldParse`
            [ Move 1 White (PlyAnnotated (Ply Pawn Nothing Nothing ('e', 4)) Nothing Nothing)
            ]

        it "возращает чёрную пешку и поле" $
          parseMove' "1... e5" `shouldParse`
            [ Move 1 Black (PlyAnnotated (Ply Pawn Nothing Nothing ('e', 5)) Nothing Nothing)
            ]

    context "[ход белых и чёрных объединены под одним номером]" $ do
      it "возращает оба хода" $ do
        parseMove' "1. e4 e5" `shouldParse`
          [ Move 1 White (PlyAnnotated (Ply Pawn Nothing Nothing ('e', 4)) Nothing Nothing)
          , Move 1 Black (PlyAnnotated (Ply Pawn Nothing Nothing ('e', 5)) Nothing Nothing)
          ]

{-
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
  -}

  describe "parsePly" $ do
    let parsePly' = parse parsePly "PGN"

    context "рокировки" $ do
      it "рокировка в сторону ферзя" $
        parsePly' "O-O-O" `shouldParse` QueensideCastling

      it "рокировка в сторону короля" $
        parsePly' "O-O" `shouldParse` KingsideCastling

    context "обычные ходы" $ do
      it "ход пешкой" $
        parsePly' "e4" `shouldParse` Ply Pawn Nothing Nothing ('e', 4)

      it "ход ладьёй" $
        parsePly' "Ra5" `shouldParse` Ply Rook Nothing Nothing ('a', 5)

      it "ход конём" $
        parsePly' "Ng2" `shouldParse` Ply Knight Nothing Nothing ('g', 2)

      it "ход слоном" $
        parsePly' "Bc7" `shouldParse` Ply Bishop Nothing Nothing ('c', 7)

      it "ход ферзём" $
        parsePly' "Qh8" `shouldParse` Ply Queen Nothing Nothing ('h', 8)

      it "ход королём" $
        parsePly' "Ka4" `shouldParse` Ply King Nothing Nothing ('a', 4)

    context "ходы со взятием" $ do
      it "взятие пешкой" $
        parsePly' "bxa5" `shouldParse` Capture Pawn (Just 'b') Nothing ('a', 5)

      it "взятие ладьёй" $
        parsePly' "Rxa5" `shouldParse` Capture Rook Nothing Nothing ('a', 5)

      it "взятие конём" $
        parsePly' "Nxg2" `shouldParse` Capture Knight Nothing Nothing ('g', 2)

      it "взятие слоном" $
        parsePly' "Bxc8" `shouldParse` Capture Bishop Nothing Nothing ('c', 8)

      it "взятие ферзём" $
        parsePly' "Qxh6" `shouldParse` Capture Queen Nothing Nothing ('h', 6)

      it "взятие королём" $
        parsePly' "Kxa3" `shouldParse` Capture King Nothing Nothing ('a', 3)

    context "ходы с указанием исходной вертикали" $ do
      it "ход ладьёй" $
        parsePly' "Rae5" `shouldParse` Ply Rook (Just 'a') Nothing ('e', 5)
     
      it "ход конём" $
        parsePly' "Nbe6" `shouldParse` Ply Knight (Just 'b') Nothing ('e', 6)
     
      it "ход слоном" $
        parsePly' "Bcf7" `shouldParse` Ply Bishop (Just 'c') Nothing ('f', 7)
     
      it "ход ферзём" $
        parsePly' "Qdg8" `shouldParse` Ply Queen (Just 'd') Nothing ('g', 8)
     
      it "ход королём" $ do
        pendingWith "король должен быть один, при указании вертикали короля, скорее всего надо падать"
        parsePly' "Kab2" `shouldParse` Ply King (Just 'a') Nothing ('b', 2)

    context "ходы с указанием исходной клетки" $ do
      it "ход ладьёй" $
        parsePly' "Rd1d2" `shouldParse` Ply Rook (Just 'd') (Just 1) ('d', 2)
     
      it "ход конём" $
        parsePly' "Nc1d3" `shouldParse` Ply Knight (Just 'c') (Just 1) ('d', 3)
     
      it "ход слоном" $
        parsePly' "Be1f2" `shouldParse` Ply Bishop (Just 'e') (Just 1) ('f', 2)
     
      it "ход ферзём" $
        parsePly' "Qe4e6" `shouldParse` Ply Queen (Just 'e') (Just 4) ('e', 6)
     
      it "ход королём" $ do
        pendingWith "король должен быть один, при указании вертикали короля, скорее всего надо падать"
        parsePly' "Kab2" `shouldParse` Ply King (Just 'a') Nothing ('b', 2)

    context "продвижение (c8=Q)" $ do
      it "белая пешка становится ферзём" $
        parsePly' "c8=Q" `shouldParse` Promotion ('c', 8) Queen

      it "белая пешка становится конём" $
        parsePly' "h8=N" `shouldParse` Promotion ('h', 8) Knight

      it "черная пешка становится ладьёй" $
        parsePly' "a1=R" `shouldParse` Promotion ('a', 1) Rook

    context "взятие с продвижением (exf8=Q)" $ do
      it "белая пешка становится ферзём" $
        parsePly' "exf8=Q" `shouldParse` CapturePromotion 'e' ('f', 8) Queen

      it "чёрная пешка становится ферзём" $
        parsePly' "hxg1=Q" `shouldParse` CapturePromotion 'h' ('g', 1) Queen

  describe "parsePlyAnnotated" $ do
    let parsePlyAnnotated' = parse parsePlyAnnotated "PGN"

    it "обычный ход" $
      parsePlyAnnotated' "Bxf6" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) Nothing Nothing

    context "шах" $ do
      it "возращает шах" $
        parsePlyAnnotated' "Bxf6+" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) (Just Check) Nothing

    context "мат" $ do
      it "возращает мат" $
        parsePlyAnnotated' "Bxf6#" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) (Just Checkmate) Nothing

    context "аннотация" $ do
      it "возращает ??" $
        parsePlyAnnotated' "Bxf6??" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) Nothing (Just "??")

      it "возращает ?!" $
        parsePlyAnnotated' "Bxf6?!" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) Nothing (Just "?!")

      it "возращает ?" $
        parsePlyAnnotated' "Bxf6?" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) Nothing (Just "?")

      it "шах и аннотация ??" $
        parsePlyAnnotated' "Bxf6+??" `shouldParse` PlyAnnotated (Capture Bishop Nothing Nothing ('f', 6)) (Just Check) (Just "??")

