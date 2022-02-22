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
        allCaptureThreatSquares White (placePieces [(('e', 4), rookWhite), (('e', 7), pawnBlack), (('a', 4), pawnBlack)] emptyBoard) `shouldBe`
          [('a', 4), ('e' , 7)]
        allCaptureThreatSquares Black (placePieces [(('d', 5), rookBlack), (('h', 5), pawnWhite), (('d', 2), pawnWhite)] emptyBoard) `shouldBe`
          [('d', 2), ('h', 5)]

    context "[ферзь]" $ do
      it "возращает список полей, которым угрожает ферзь" $ do
        allCaptureThreatSquares White (placePiece ('d', 4) queenWhite initialBoard) `shouldBe`
          [('g', 7), ('a' , 7), ('d', 7)]
        allCaptureThreatSquares Black (placePieces [(('d', 5), rookBlack), (('h', 5), pawnWhite), (('d', 2), pawnWhite)] emptyBoard) `shouldBe`
          [('d', 2), ('h', 5)]

    context "[король]" $ do
      it "возращает список полей, которым угрожает король" $ do
        let board = placePieces
              [ (('e', 6), kingWhite)
              , (('d', 7), pawnBlack)
              , (('e', 7), pawnBlack)
              , (('f', 7), pawnBlack)] emptyBoard
        allCaptureThreatSquares White board `shouldBe` [('f', 7), ('d', 7), ('e', 7)]

      it "не угрожает фигурам, которые находятся под защитой ферзя" $ do
        let board = placePieces
              [ (('e', 6), kingWhite)
              , (('d', 7), pawnBlack)
              , (('e', 7), pawnBlack)
              , (('f', 7), pawnBlack)
              , (('e', 8), queenBlack)] emptyBoard
        allCaptureThreatSquares White board `shouldBe` []

      it "не угрожает фигурам, которые находятся под защитой короля" $ do
        let board = placePieces
              [ (('e', 6), kingWhite)
              , (('d', 7), pawnBlack)
              , (('e', 7), pawnBlack)
              , (('f', 7), pawnBlack)
              , (('e', 8), kingBlack)] emptyBoard
        allCaptureThreatSquares White board `shouldBe` []

  describe "isCheck" $ do
    it "возращает False в начальной позиции" $ do
      isCheck White initialBoard `shouldBe` False
      isCheck Black initialBoard `shouldBe` False

    context "[пешка]" $ do
      it "возращает False, если пешка не шахует короля" $ do
        isCheck Black (placePieces [(('e', 7), pawnWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

      it "возращает True, если пешка шахует короля" $ do
        isCheck Black (placePieces [(('d', 7), pawnWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

    context "[конь]" $ do
      it "возращает True, если конь угрожает королю противника" $ do
        isCheck Black (placePieces [(('d', 6), knightWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

      it "возращает True, если короля 'прикрывает' фигура противника" $ do
        isCheck Black (placePiece ('d', 6) knightWhite initialBoard) `shouldBe` True

      it "возращает False, если конь может сходить на поле своего же короля" $ do
        isCheck Black (placePieces [(('d', 6), knightBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

    context "[слон]" $ do
      it "возращает True, если слон угрожает королю противника" $ do
        isCheck Black (placePieces [(('a', 4), bishopWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [(('a', 4), bishopWhite), (('d', 7), pawnBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

      it "возращает False, если свой слон может сходить на поле короля" $ do
        isCheck Black (placePieces [(('a', 4), bishopBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

    context "[ладья]" $ do
      it "возращает True, если ладья угрожает королю противника" $ do
        isCheck Black (placePieces [(('e', 1), rookWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True
        isCheck Black (placePieces [(('a', 8), rookWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [(('e', 1), rookWhite), (('e', 7), pawnBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

      it "возращает False, если своя ладья может сходить на поле короля" $ do
        isCheck Black (placePieces [(('e', 1), rookBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

    context "[ферзь]" $ do
      it "возращает True, если ферзь угрожает королю противника" $ do
        isCheck Black (placePieces [(('e', 1), queenWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True
        isCheck Black (placePieces [(('a', 8), queenWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True
        isCheck Black (placePieces [(('b', 5), queenWhite), (('e', 8), kingBlack)] emptyBoard) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [(('e', 1), queenWhite), (('e', 7), pawnBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False
        isCheck Black (placePieces [(('b', 5), queenWhite), (('d', 7), pawnBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

      it "возращает False, если своя ладья может сходить на поле короля" $ do
        isCheck Black (placePieces [(('e', 1), rookBlack), (('e', 8), kingBlack)] emptyBoard) `shouldBe` False

     -- Король не может шаховать

  describe "isDeadPosition" $ do
    it "возращает True, если на доске два короля" $ do
      let board = placePieces [(('e', 4), kingWhite), (('a', 8), kingBlack)] emptyBoard
      isDeadPosition White board `shouldBe` True
      isDeadPosition Black board `shouldBe` True

    it "возращает True, если на доске два короля и слон https://lichess.org/editor/8/2b2k2/8/8/6K1/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('g', 4), kingWhite), (('f', 7), kingBlack), (('c', 7), bishopBlack)] emptyBoard
      isDeadPosition White board `shouldBe` True
      isDeadPosition Black board `shouldBe` True
      




  describe "isStalemate" $ do
    it "возвращает True, в случае пата https://lichess.org/editor/7k/5K2/6Q1/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('f', 7), kingWhite), (('g', 6), queenWhite), (('h', 8), kingBlack)] emptyBoard
      isStalemate Black board `shouldBe` True

    it "возвращает False, в случае мата https://lichess.org/editor/7k/5KQ1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('f', 7), kingWhite), (('g', 7), queenWhite), (('h', 8), kingBlack)] emptyBoard
      isStalemate Black board `shouldBe` False

    it "возвращает False, в случае шаха https://lichess.org/editor/7k/4K1Q1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('e', 7), kingWhite), (('g', 7), queenWhite), (('h', 8), kingBlack)] emptyBoard
      isStalemate Black board `shouldBe` False

  describe "isMate" $ do
    it "возвращает False, в случае пата https://lichess.org/editor/7k/5K2/6Q1/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('f', 7), kingWhite), (('g', 6), queenWhite), (('h', 8), kingBlack)] emptyBoard
      isMate Black board `shouldBe` False

    it "возвращает True, в случае мата https://lichess.org/editor/7k/5KQ1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('f', 7), kingWhite), (('g', 7), queenWhite), (('h', 8), kingBlack)] emptyBoard
      isMate Black board `shouldBe` True

    it "возвращает False, в случае шаха https://lichess.org/editor/7k/4K1Q1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [(('e', 7), kingWhite), (('g', 7), queenWhite), (('h', 8), kingBlack)] emptyBoard
      isMate Black board `shouldBe` False