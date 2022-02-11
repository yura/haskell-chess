module Laws.RookSpec (spec) where

import Test.Hspec

import           Board
import           Board.InitialPosition
import           Laws.Rook

spec :: Spec
spec = do
  describe "rookMoves" $ do
    it "все ходы ладьи, если ладья находится в центре" $ do
      rookMoves ('d', 4) `shouldBe`
        [
          ('c', 4), ('b', 4), ('a', 4)
        , ('d', 3), ('d', 2), ('d', 1)
        , ('e', 4), ('f', 4), ('g', 4), ('h', 4)
        , ('d', 5), ('d', 6), ('d', 7), ('d', 8)
        ]

  describe "captureThreatSquares" $ do
    it "все угрозы ладьёй фигурам противника" $ do
      captureThreatSquares White ('b', 2) initialBoard `shouldBe` [('b', 7)]

    it "возращает пустой список если нет фигур, которым угрожает ладья" $ do
      captureThreatSquares White ('a', 1) initialBoard `shouldBe` []
