module EvaluationSpec (spec) where

import Test.Hspec

import Evaluation
import Board
import Board.InitialPosition

spec :: Spec
spec = do
  describe "evaluatePosition" $ do
    it "для равной позиции возращает 0" $
      evaluatePosition initialBoard `shouldBe` 0

    it "возращает положительное число, если позиция лучше для игрока, у которого следующий ход" $
      evaluatePosition (placePiece ('d', 1) queenWhite emptyBoard) `shouldBe` 9

    it "возращает отрицательное число, если позиция хуже для игрока, у которого следующий ход" $ do
      let board = placePiece ('d', 4) queenBlack initialBoard
      evaluatePosition board `shouldBe` -9

    it "в случае мата чёрным, возвращает минимальное значение" $ do
      let board = placePieces [(('f', 7), kingWhite), (('g', 7), queenWhite), (('h', 8), kingBlack)] emptyBoard
      evaluatePosition board { nextMove = Black } `shouldBe` maxBound 

    it "в случае ничьи возращает 0" $ do
      let board = placePieces [(('f', 7), kingWhite), (('a', 2), bishopWhite), (('h', 8), kingBlack)] emptyBoard
      evaluatePosition board { nextMove = Black } `shouldBe` 0 
