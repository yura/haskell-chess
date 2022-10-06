module Format.FENSpec (spec) where

import Test.Hspec

import Board
import Board.InitialPosition
import Format.FEN

spec :: Spec
spec = do
  describe "squaresFENOrder" $ do
    it "возвращает 8 рядов" $
      length squaresFENOrder `shouldBe` 8

    it "первое поле должено быть 'a8'" $
      head (head squaresFENOrder) `shouldBe` Square 56

    it "последнее поле в первом ряду должено быть 'h8'" $
      last (head squaresFENOrder) `shouldBe` Square 63

    it "первое поле в последнем ряду должено быть 'a1'" $
      head (last squaresFENOrder) `shouldBe` Square 0

    it "последнее поле должено быть 'h1'" $
      last (last squaresFENOrder) `shouldBe` Square 7

  describe "nextMoveFEN" $ do
     it "всегда возращает следующий ход белых" $
       nextMoveFEN initialBoard `shouldBe` "w"

  describe "toFEN" $ do
    it "пустая доска даёт пустую позицию" $ do
      toFEN emptyBoard `shouldBe` "8/8/8/8/8/8/8/8 w - - 0 1"

    it "начальная позиция" $
      toFEN initialBoard `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    it "позиция после двух ходов" $ do
      let board = move (move initialBoard (Move pawnWhite (read "e2") (read "e4"))) (Move pawnBlack (read "e7") (read "e5"))
      toFEN board `shouldBe` "rnbqkbnr/pppp1ppp/8/4p3/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2"

    it "чёрный король на a8, белый - на a1, белая пешка на a2, ход белых" $ do
      let board = placePieces [((read "a2"), Piece Pawn White), ((read "a1"), Piece King White), ((read "a8"), Piece King Black)] emptyBoard
      toFEN board `shouldBe` "k7/8/8/8/8/8/P7/K7 w - - 0 1"

