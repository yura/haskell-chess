{-# LANGUAGE NoOverloadedStrings #-}

module Format.PGN.ExportSpec (spec) where

import Test.Hspec
import Test.Hspec.Parsec
import Text.Parsec

import Format.PGN.Export
import Board

spec :: Spec
spec = do
  describe "squareToPGN" $ do
    it "переводит значение пары в строку" $ do
      squareToPGN (read "e2") `shouldBe` "e2"
  
  describe "moveToPGN" $ do
    context "[рокировка]" $ do
      it "рокировка в сторону ферзя" $
        moveToPGN (QueensideCastling White) `shouldBe` "O-O-O"

      it "рокировка в сторону короля" $
        moveToPGN (KingsideCastling White) `shouldBe` "O-O"

    context "[обычный ход]" $ do
      it "преобразует ход пешки в строку" $ do
        moveToPGN (Move pawnWhite (read "e2") (read "e4")) `shouldBe` "e2e4"

      it "преобразует ход коня в строку" $ do
        moveToPGN (Move knightBlack (read "a3") (read "b5")) `shouldBe` "Na3b5"

      it "преобразует ход слона в строку" $ do
        moveToPGN (Move bishopWhite (read "d3") (read "e4")) `shouldBe` "Bd3e4"

      it "преобразует ход ладьи в строку" $ do
        moveToPGN (Move rookBlack (read "g5") (read "g8")) `shouldBe` "Rg5g8"

      it "преобразует ход ферзя в строку" $ do
        moveToPGN (Move queenWhite (read "a1") (read "h8")) `shouldBe` "Qa1h8"

      it "преобразует ход короля в строку" $ do
        moveToPGN (Move kingBlack (read "a1") (read "b1")) `shouldBe` "Ka1b1"
    
    context "[взятие]" $ do
      it "взятие пешкой" $ do
        moveToPGN (Capture pawnWhite (read "a2") (read "b3")) `shouldBe` "a2xb3"

      it "взятие на проходе" $ do
        moveToPGN (EnPassantCapture pawnWhite (read "d5") (read "e6")) `shouldBe` "d5xe6"

      it "другой фигурой (кроме пешки)" $ do
        moveToPGN (Capture queenWhite (read "a2") (read "b3")) `shouldBe` "Qa2xb3"
    
    context "[превращение]" $ do
      it "в ферзя" $
        moveToPGN (Promotion (read "b7") (read "b8") queenWhite) `shouldBe` "b7b8=Q"

      it "в ладью" $
        moveToPGN (Promotion (read "c2") (read "c1") rookBlack) `shouldBe` "c2c1=R"
      
      it "со взятием" $
        moveToPGN (CapturePromotion (read "h2") (read "g1") knightBlack) `shouldBe` "h2xg1=N"

  describe "exportMovesToPGN" $ do
    it "возращает строку из списка ходов" $ do
      let moves = 
            [ Move    pawnWhite (read "e2") (read "e4")
            , Move    pawnBlack (read "d7") (read "d5")
            , Capture pawnWhite (read "e4") (read "d5")]
      movesToPGN moves `shouldBe` "1. e2e4 d7d5 2. e4xd5"

