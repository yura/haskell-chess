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

  describe "allCaptureThreatSquares" $ do
    context "[белая пешка]" $ do
      it "возращает список всех полей, которым угрожает белая пешка" $ do
        allCaptureThreatSquares White (placePiece ('e', 6) pawnWhite initialBoard) `shouldBe` [('d', 7), ('f' , 7)]

      it "возращает список всех полей, которым угрожают две белые пешки" $ do
        allCaptureThreatSquares White (placePieces [(('e', 6), pawnWhite), (('d', 6), pawnWhite)] initialBoard) `shouldBe` [('c',7),('e',7),('d',7),('f',7)]

    context "[чёрная пешка]" $ do
      it "возращает список всех полей, которым угрожает чёрная пешка" $ do
        allCaptureThreatSquares Black (placePiece ('e', 3) pawnBlack initialBoard) `shouldBe` [('d', 2), ('f' , 2)]

      it "возращает список всех полей, которым угрожают две черные пешки" $ do
        allCaptureThreatSquares Black (placePieces [(('e', 3), pawnBlack), (('d', 3), pawnBlack)] initialBoard) `shouldBe` [('c',2),('e',2),('d',2),('f',2)]

    context "[конь]" $ do
      it "возращает список полей, которым угрожает конь" $ do
        allCaptureThreatSquares White (placePiece ('e', 6) knightWhite initialBoard) `shouldBe` [('g', 7), ('f' , 8), ('d', 8), ('c', 7)]
        allCaptureThreatSquares Black (placePiece ('e', 2) knightBlack initialBoard) `shouldBe` [('c', 1), ('g' , 1)]

    context "[слон]" $ do
      it "возращает список полей, которым угрожает слон" $ do
        allCaptureThreatSquares White (placePieces [(('e', 4), bishopWhite), (('b', 7), pawnBlack), (('h', 7), pawnBlack)] emptyBoard) `shouldBe`
          [('h', 7), ('b' , 7)]
        allCaptureThreatSquares Black (placePieces [(('e', 4), bishopBlack), (('c', 2), pawnWhite), (('g', 2), pawnWhite)] emptyBoard) `shouldBe`
          [('c', 2), ('g', 2)]

    context "[ладья]" $ do
      it "возращает список полей, которым угрожает ладья" $ do
        allCaptureThreatSquares White (placePieces [(('e', 4), bishopWhite), (('b', 7), pawnBlack), (('h', 7), pawnBlack)] emptyBoard) `shouldBe`
          [('h', 7), ('b' , 7)]
        allCaptureThreatSquares Black (placePieces [(('e', 4), bishopBlack), (('c', 2), pawnWhite), (('g', 2), pawnWhite)] emptyBoard) `shouldBe`
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


