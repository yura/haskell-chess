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
        evaluatePosition (placePiece (read "d1") queenWhite emptyBoard) `shouldBe` 9

      it "возращает отрицательное число, если позиция хуже для игрока, у которого следующий ход" $ do
        let board = placePiece (read "d4") queenBlack initialBoard
        evaluatePosition board `shouldBe` -9

      it "в случае мата белым, возвращает минимальное значение" $
        evaluatePosition whiteMatedWithKnight `shouldBe` mateValue 

      it "в случае ничьи возращает 0" $ do
        let board = placePieces [((read "f7"), kingWhite), ((read "a2"), bishopWhite), ((read "h8"), kingBlack)] emptyBoard
        evaluatePosition board `shouldBe` 0 

    context "[ход чёрных]" $ do
      it "для равной позиции возращает 0" $
        evaluatePosition initialBoard { nextMove = White } `shouldBe` 0

      it "возращает положительное число, если позиция лучше для игрока, у которого следующий ход" $ do
        let board = placePiece (read "d1") queenBlack emptyBoard
        evaluatePosition board { nextMove = Black } `shouldBe` 9

      it "возращает отрицательное число, если позиция хуже для игрока, у которого следующий ход" $ do
        let board = placePiece (read "d4") queenWhite initialBoard
        evaluatePosition board { nextMove = Black } `shouldBe` -9

      it "в случае мата чёрным, возвращает минимальное значение" $ do
        let board = placePieces [((read "f7"), kingWhite), ((read "g7"), queenWhite), ((read "h8"), kingBlack)] emptyBoard
        evaluatePosition board { nextMove = Black } `shouldBe` mateValue 

      it "в случае ничьи возращает 0" $ do
        let board = placePieces [((read "f7"), kingWhite), ((read "a2"), bishopWhite), ((read "h8"), kingBlack)] emptyBoard
        evaluatePosition board { nextMove = Black } `shouldBe` 0
