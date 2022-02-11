module LawsSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map

import           Board
import           Board.InitialPosition
import           Laws

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

  describe "kingMoves" $ do
    it "все ходы короля, если король находится в центре" $ do
      kingMoves ('h', 6) `shouldBe`
        [
          ('g', 5), ('g', 7)
        , ('g', 6), ('h', 5), ('h', 7)
        ]

  describe "kingPossibleMoves" $ do
    context "[рокировка]" $ do
      it "не ходит через клетку вперед, если поле занято своей фигурой" $ pending
      it "не ходит через клетку вперед, если поле занято фигурой противника" $ pending

  describe "kingPossibleMoves" $ do
    context "[рокировка]" $ do
      it "не ходит под шах" $ pending
      it "не встречается с королём (частный случай шаха)" $ pending
      it "не рокируется, если ходил" $ pending
      it "не рокируется, если ходила ладья?" $ pending

    it "не ходит под шах" $ pending

  describe "beatenSquares" $ do
    context "[белая пешка]" $ do
      it "возращает список всех полей, которые бьёт белая пешка" $ do
        beatenSquares White (placePiece ('e', 2) pawnWhite emptyBoard) `shouldBe` [('d', 3), ('f' , 3)]

      it "возращает список всех полей, которые бьют две белые пешки" $ do
        beatenSquares White (placePieces [(('e', 2), pawnWhite), (('d', 2), pawnWhite)] emptyBoard) `shouldBe` [('c',3),('e',3),('d',3),('f',3)]

    context "[чёрная пешка]" $ do
      it "возращает список всех полей, которые бьёт чёрная пешка" $ do
        beatenSquares Black (placePiece ('e', 7) pawnBlack emptyBoard) `shouldBe` [('d', 6), ('f' , 6)]

      it "возращает список всех полей, которые бьют две черные пешки" $ do
        beatenSquares Black (placePieces [(('e', 7), pawnBlack), (('d', 7), pawnBlack)] emptyBoard) `shouldBe` [('c',6),('e',6),('d',6),('f',6)]

    context "[конь]" $ do
      it "возращает список полей, на которые может сходить конь" $ do
        beatenSquares White (placePiece ('e', 2) knightWhite emptyBoard) `shouldBe` [('c', 1), ('g' , 1), ('g', 3), ('f', 4), ('d', 4), ('c', 3)]
        beatenSquares Black (placePiece ('e', 2) knightBlack emptyBoard) `shouldBe` [('c', 1), ('g' , 1), ('g', 3), ('f', 4), ('d', 4), ('c', 3)]

    context "[слон]" $ do
      it "возращает список полей, на которые может бить слон, если доска пустая" $ do
        beatenSquares White (placePieces [(('e', 4), bishopWhite), (('b', 7), pawnBlack), (('h', 7), pawnBlack)] emptyBoard) `shouldBe`
          [('h', 7), ('b' , 7)]
        beatenSquares Black (placePieces [(('e', 4), bishopBlack), (('c', 2), pawnWhite), (('g', 2), pawnWhite)] emptyBoard) `shouldBe`
          [('c', 2), ('g', 2)]

  describe "isCheck" $ do
    it "возращает False в начальной позиции" $ do
      isCheck White initialBoard `shouldBe` False
      isCheck Black initialBoard `shouldBe` False

    it "возращает False, если пешка не шахует короля" $ do
      isCheck Black (placePieces [(('e', 7), pawnWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

    it "возращает True, если пешка шахует короля" $ do
      isCheck Black (placePieces [(('d', 7), pawnWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

    context "[слон]" $ do
      it "возращает True, если слон противника может сходить на поле короля" $ do
        isCheck Black (placePieces [(('a', 4), bishopWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [(('a', 4), bishopWhite), (('d', 7), pawnBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

      it "возращает False, если свой слон может сходить на поле короля" $ do
        isCheck Black (placePieces [(('a', 4), bishopBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False


