module Format.FENSpec (spec) where

import Test.Hspec

import Board
import Board.InitialPosition
import Format.FEN

spec :: Spec
spec = do
  describe "fenSquares" $ do
    it "возвращает 8 рядов" $
      length fenSquares `shouldBe` 8

    it "первое поле должено быть 'a8'" $
      (head $ head $ fenSquares) `shouldBe` ('a', 8)

    it "последнее поле должено быть 'h1'" $
      (last $ last $ fenSquares) `shouldBe` ('h', 1)

  describe "nextMove" $ do
     it "всегда возращает следующий ход белых" $
       nextMove `shouldBe` "w"

  describe "exportFEN" $ do
    it "пустая доска даёт пустую позицию" $ do
      exportFEN emptyBoard `shouldBe` "8/8/8/8/8/8/8/8 w - - 0 1"

    it "чёрный король на a8, белый - на a1, белая пешка на a2, ход белых" $ do
      let board = placePieces emptyBoard [(('a', 2), Piece Pawn White), (('a', 1), Piece King White), (('a', 8), Piece King Black)]
      exportFEN board `shouldBe` "k7/8/8/8/8/8/P7/K7 w - - 0 1"

    it "начальная позиция" $
      exportFEN initialBoard `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

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

