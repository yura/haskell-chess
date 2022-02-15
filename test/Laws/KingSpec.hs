module Laws.KingSpec (spec) where

import           Test.Hspec

import           Laws
import           Laws.King

import           Board
import           Board.InitialPosition

spec :: Spec
spec = do
  describe "moveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      moveSquares ('h', 6) `shouldBe`
        [
          ('g', 5), ('g', 7)
        , ('g', 6), ('h', 5), ('h', 7)
        ]

  describe "kingValidMoveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      kingValidMoveSquares White ('h', 4) initialBoard `shouldBe`
        [ ('g', 3), ('g', 5)
        , ('g', 4), ('h', 3), ('h', 5)
        ]

    it "не может приближаться к пешкам" $ do
      kingValidMoveSquares White ('h', 5) initialBoard `shouldBe`
        [ ('g', 4)
        , ('g', 5), ('h', 4)
        ]

    context "[рокировка]" $ do
      it "не ходит через клетку вперед, если поле занято своей фигурой" $ pending
      it "не ходит через клетку вперед, если поле занято фигурой противника" $ pending
      it "не ходит под шах" $ pending
      it "не встречается с королём (частный случай шаха)" $ pending
      it "не рокируется, если ходил" $ pending
      it "не рокируется, если ходила ладья?" $ pending

