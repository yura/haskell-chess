module Laws.QueenSpec (spec) where

import           Test.Hspec

import           Board
import           Board.InitialPosition
import           Laws.Queen

spec :: Spec
spec = do
  describe "queenMoves" $ do
    it "все ходы ферзя, если ферзь находится в центре" $ do
      queenMoves ('e', 7) `shouldBe`
        [
          ('d', 6), ('c', 5), ('b', 4), ('a', 3)
        , ('f', 6), ('g', 5), ('h', 4)
        , ('f', 8)
        , ('d', 8)
        , ('d', 7), ('c', 7), ('b', 7), ('a', 7)
        , ('e', 6), ('e', 5), ('e', 4), ('e', 3), ('e', 2), ('e', 1)
        , ('f', 7), ('g', 7), ('h', 7)
        , ('e', 8)
        ]

  describe "captureThreatSquares" $ do
    it "все угрозы ладьёй фигурам противника" $ do
      captureThreatSquares White ('b', 2) initialBoard `shouldBe` [('g', 7), ('b', 7)]

    it "возращает пустой список если нет фигур, которым угрожает ладья" $ do
      captureThreatSquares White ('a', 1) initialBoard `shouldBe` []
