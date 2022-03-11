module EvaluationSpec (spec) where

import Test.Hspec
import Fixtures

import Evaluation
import Board
import Board.InitialPosition

spec :: Spec
spec = do
  describe "evaluatePosition" $ do
    context "[ход белых]" $ do
      it "для равной позиции возращает 0" $
        evaluatePosition initialBoard `shouldBe` 0

      it "возращает положительное число, если позиция лучше для игрока, у которого следующий ход" $
        evaluatePosition (placePiece ('d', 1) queenWhite emptyBoard) `shouldBe` 9

      it "возращает отрицательное число, если позиция хуже для игрока, у которого следующий ход" $ do
        let board = placePiece ('d', 4) queenBlack initialBoard
        evaluatePosition board `shouldBe` -9

      it "в случае мата белым, возвращает минимальное значение" $
        evaluatePosition whiteMatedWithKnight `shouldBe` mateValue 

      it "в случае ничьи возращает 0" $ do
        let board = placePieces [(('f', 7), kingWhite), (('a', 2), bishopWhite), (('h', 8), kingBlack)] emptyBoard
        evaluatePosition board `shouldBe` 0 

    context "[ход чёрных]" $ do
      it "для равной позиции возращает 0" $
        evaluatePosition initialBoard { nextMove = White } `shouldBe` 0

      it "возращает положительное число, если позиция лучше для игрока, у которого следующий ход" $ do
        let board = placePiece ('d', 1) queenBlack emptyBoard
        evaluatePosition board { nextMove = Black } `shouldBe` 9

      it "возращает отрицательное число, если позиция хуже для игрока, у которого следующий ход" $ do
        let board = placePiece ('d', 4) queenWhite initialBoard
        evaluatePosition board { nextMove = Black } `shouldBe` -9

      it "в случае мата чёрным, возвращает минимальное значение" $ do
        let board = placePieces [(('f', 7), kingWhite), (('g', 7), queenWhite), (('h', 8), kingBlack)] emptyBoard
        evaluatePosition board { nextMove = Black } `shouldBe` mateValue 

      it "в случае ничьи возращает 0" $ do
        let board = placePieces [(('f', 7), kingWhite), (('a', 2), bishopWhite), (('h', 8), kingBlack)] emptyBoard
        evaluatePosition board { nextMove = Black } `shouldBe` 0
