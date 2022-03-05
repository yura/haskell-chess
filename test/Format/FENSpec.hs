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
      head (head squaresFENOrder) `shouldBe` ('a', 8)

    it "последнее поле в первом ряду должено быть 'h8'" $
      last (head squaresFENOrder) `shouldBe` ('h', 8)

    it "первое поле в последнем ряду должено быть 'a1'" $
      head (last squaresFENOrder) `shouldBe` ('a', 1)

    it "последнее поле должено быть 'h1'" $
      last (last squaresFENOrder) `shouldBe` ('h', 1)

  describe "nextMoveFEN" $ do
     it "всегда возращает следующий ход белых" $
       nextMoveFEN initialBoard `shouldBe` "w"

  describe "exportToFEN" $ do
    it "пустая доска даёт пустую позицию" $ do
      toFEN emptyBoard `shouldBe` "8/8/8/8/8/8/8/8 w - - 0 1"

    it "начальная позиция" $
      toFEN initialBoard `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

    it "чёрный король на a8, белый - на a1, белая пешка на a2, ход белых" $ do
      let board = placePieces [(('a', 2), Piece Pawn White), (('a', 1), Piece King White), (('a', 8), Piece King Black)] emptyBoard
      toFEN board `shouldBe` "k7/8/8/8/8/8/P7/K7 w - - 0 1"

  describe "incrementLastNumber" $ do
    it "возвращает 1 если строка пустая" $
      incrementLastNumberOrAdd1 "" `shouldBe` "1"

    it "добавляет 1 если строка оканчивается на букву" $
      incrementLastNumberOrAdd1 "k" `shouldBe` "k1"

    it "увеличивает последнее число на 1" $
      incrementLastNumberOrAdd1 "hello7" `shouldBe` "hello8"

  describe "incrementLastNumber" $ do
    it "увеличивает последнее число на 1" $
      incrementLastNumber "hello1" `shouldBe` "hello2"

