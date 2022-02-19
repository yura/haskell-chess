{-# LANGUAGE NoOverloadedStrings #-}

module Format.PGN.ExportSpec (spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Format.PGN.Export
import Board

spec :: Spec
spec = do
  describe "exportSquareToPGN" $ do
    it "переводит значение пары в строку" $ do
      exportSquareToPGN ('e', 2) `shouldBe` "e2"
  
  describe "exportMoveToPGN" $ do
    context "[рокировка]" $ do
      it "рокировка в сторону ферзя" $
        exportMoveToPGN (QueensideCastling White) `shouldBe` "O-O-O"

      it "рокировка в сторону короля" $
        exportMoveToPGN (KingsideCastling White) `shouldBe` "O-O"

    context "[обычный ход]" $ do
      it "преобразует ход пешки в строку" $ do
        exportMoveToPGN (Move pawnWhite ('e', 2) ('e', 4)) `shouldBe` "e2e4"

      it "преобразует ход коня в строку" $ do
        exportMoveToPGN (Move knightBlack ('a', 3) ('b', 5)) `shouldBe` "Na3b5"

      it "преобразует ход слона в строку" $ do
        exportMoveToPGN (Move bishopWhite ('d', 3) ('e', 4)) `shouldBe` "Bd3e4"

      it "преобразует ход ладьи в строку" $ do
        exportMoveToPGN (Move rookBlack ('g', 5) ('g', 8)) `shouldBe` "Rg5g8"

      it "преобразует ход ферзя в строку" $ do
        exportMoveToPGN (Move queenWhite ('a', 1) ('h', 8)) `shouldBe` "Qa1h8"

      it "преобразует ход короля в строку" $ do
        exportMoveToPGN (Move kingBlack ('a', 1) ('b', 1)) `shouldBe` "Ka1b1"
    
    context "[взятие]" $ do
      it "взятие пешкой" $ do
        exportMoveToPGN (Capture pawnWhite ('a', 2) ('b', 3)) `shouldBe` "a2xb3"

      it "взятие на проходе" $ do
        exportMoveToPGN (EnPassantCapture pawnWhite ('d', 5) ('e', 6)) `shouldBe` "d5xe6"

      it "другой фигурой (кроме пешки)" $ do
        exportMoveToPGN (Capture queenWhite ('a', 2) ('b', 3)) `shouldBe` "Qa2xb3"
    
    context "[превращение]" $ do
      it "в ферзя" $
        exportMoveToPGN (Promotion ('b', 7) ('b', 8) queenWhite) `shouldBe` "b7b8=Q"

      it "в ладью" $
        exportMoveToPGN (Promotion ('c', 2) ('c', 1) rookBlack) `shouldBe` "c2c1=R"
      
      it "со взятием" $
        exportMoveToPGN (CapturePromotion ('h', 2) ('g', 1) knightBlack) `shouldBe` "h2xg1=N"

  describe "exportMovesToPGN" $ do
    it "возращает строку из списка ходов" $ do
      let moves = 
            [ Move    pawnWhite ('e', 2) ('e', 4)
            , Move    pawnBlack ('d', 7) ('d', 5)
            , Capture pawnWhite ('e', 4) ('d', 5)]
      exportMovesToPGN moves `shouldBe` "1. e2e4 d7d5 2. e4xd5"

