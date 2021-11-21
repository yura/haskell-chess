module Format.FENSpec (spec) where

import Test.Hspec

import Board
import Format.FEN

spec :: Spec
spec = do
  describe "exportFEN" $ do
    it "пустая доска даёт пустую позицию" $ do
      pending
      exportFEN emptyBoard `shouldBe` "8/8/8/8/8/8/8/8 w - - 0 1"

    it "чёрный король на a8, белый - на a1, белая пешка на a2, ход белых" $ do
      pending
      exportFEN emptyBoard `shouldBe` "k7/8/8/8/8/8/P7/K7 w - - 0 1"

    it "начальная позиция" $ pending
      -- exportFEN initialPosition `shouldBe` "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

