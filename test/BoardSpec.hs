module BoardSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map
import           Board
import           Board.InitialPosition

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "генерирует пустую доску размером 0 клеток" $
      length (squares emptyBoard) `shouldBe` 0

  describe "takenBy" $ do
    context "[поле не занято ни белыми, ни чёрными]" $ do
      it "возращает False, при запросе занято ли белыми" $ do
        takenBy White emptyBoard ('e', 2) `shouldBe` False

      it "возращает False, при запросе занято ли чёрными" $ do
        takenBy Black emptyBoard ('e', 2) `shouldBe` False

    context "[поле занято белыми]" $ do
      it "возращает True, при запросе занято ли белыми" $ do
        takenBy White initialBoard ('e', 2) `shouldBe` True

      it "возращает False, при запросе занято ли чёрными" $ do
        takenBy Black initialBoard ('e', 2) `shouldBe` False

    context "[поле занято чёрными]" $ do
      it "возращает False, при запросе занято ли белыми" $ do
        takenBy White initialBoard ('e', 7) `shouldBe` False

      it "возращает True, при запросе занято ли чёрными" $ do
        takenBy Black initialBoard ('e', 7) `shouldBe` True

  describe "pawnSquares" $ do
    it "возращает пустой список, если доска пустая белых пешек" $
      pawnSquares White emptyBoard `shouldBe` []

    it "возращает пустой список, если на доске не пешек данного цвета" $ do
      let board = placePiece ('e', 2) pawnWhite emptyBoard
      pawnSquares Black board `shouldBe` []

    it "возращает все поля занятые пешками данного цвета" $ do
      let board = placePieces [(('d', 2), pawnWhite), (('e', 2), pawnWhite)] emptyBoard
      pawnSquares White board `shouldBe` [('e', 2), ('d', 2)]

  describe "placePiece" $ do
    it "ставит фигуру на заданную клетку" $ do
      let sqs = squares $ placePiece ('d', 4) pawnWhite emptyBoard 
      M.lookup ('d', 4) sqs `shouldBe` Just pawnWhite

  describe "placePieces" $ do
    it "ставит фигуры на заданные клетки" $ do
      let sqs = squares $ placePieces [(('a', 2), pawnWhite), (('a', 7), pawnBlack)] emptyBoard 
      M.lookup ('a', 2) sqs `shouldBe` Just pawnWhite
      M.lookup ('a', 7) sqs `shouldBe` Just pawnBlack

  describe "kingAt" $ do
    it "возращает позицию короля" $ do
      kingAt White initialBoard `shouldBe` ('e', 1)
      kingAt Black initialBoard `shouldBe` ('e', 8)

  describe "disableKingsideCastling" $ do
    context "белые" $ do
      it "выставляет поле whiteCanCastleKingside в False" $
        whiteCanCastleKingside (disableKingsideCastling White initialBoard) `shouldBe` False

      it "поле whiteCanCastleQueenside без изменений" $
        whiteCanCastleQueenside (disableKingsideCastling White initialBoard) `shouldBe` True

      it "поле blackCanCastleKingside без изменения" $
        blackCanCastleKingside (disableKingsideCastling White initialBoard) `shouldBe` True

      it "поле blackCanCastleQueenside без изменения" $
        blackCanCastleQueenside (disableKingsideCastling White initialBoard) `shouldBe` True

    context "[чёрные]" $ do
      it "выставляет поле blackCanCastleKingside в False" $
        blackCanCastleKingside (disableKingsideCastling Black initialBoard) `shouldBe` False

      it "поле blackCanCastleQueenside без изменения" $
        blackCanCastleQueenside (disableKingsideCastling Black initialBoard) `shouldBe` True

      it "поле whiteCanCastleKingside без изменения" $
        whiteCanCastleKingside (disableKingsideCastling Black initialBoard) `shouldBe` True

      it "поле whiteCanCastleQueenside без изменений" $
        whiteCanCastleQueenside (disableKingsideCastling Black initialBoard) `shouldBe` True

  describe "disableQueensideCastling" $ do
    context "белые" $ do
      it "выставляет поле whiteCanCastleQueenside в False" $
        whiteCanCastleQueenside (disableQueensideCastling White initialBoard) `shouldBe` False

      it "поле whiteCanCastleKingside без изменений" $
        whiteCanCastleKingside (disableQueensideCastling White initialBoard) `shouldBe` True

      it "поле blackCanCastleKingside без изменения" $
        blackCanCastleKingside (disableQueensideCastling White initialBoard) `shouldBe` True

      it "поле blackCanCastleQueenside без изменения" $
        blackCanCastleQueenside (disableQueensideCastling White initialBoard) `shouldBe` True

    context "[чёрные]" $ do
      it "выставляет поле blackCanCastleQueenside в False" $
        blackCanCastleQueenside (disableQueensideCastling Black initialBoard) `shouldBe` False

      it "поле blackCanCastleKingside " $
        blackCanCastleKingside (disableQueensideCastling Black initialBoard) `shouldBe` True

      it "поле whiteCanCastleKingside без изменения" $
        whiteCanCastleKingside (disableQueensideCastling Black initialBoard) `shouldBe` True

      it "поле whiteCanCastleQueenside без изменений" $
        whiteCanCastleQueenside (disableQueensideCastling Black initialBoard) `shouldBe` True

  describe "disableCastling" $ do
    context "[белые]" $ do
      it "выставляет поле whiteCanCastleKingside в False" $
        whiteCanCastleKingside (disableCastling White initialBoard) `shouldBe` False

      it "выставляет поле whiteCanCastleQueenside в False" $
        whiteCanCastleQueenside (disableCastling White initialBoard) `shouldBe` False

      it "поле blackCanCastleKingside без изменения" $
        blackCanCastleKingside (disableCastling White initialBoard) `shouldBe` True

      it "поле blackCanCastleQueenside без изменения" $
        blackCanCastleQueenside (disableCastling White initialBoard) `shouldBe` True

    context "[чёрные]" $ do
      it "выставляет поле blackCanCastleKingside в False" $
        blackCanCastleKingside (disableCastling Black initialBoard) `shouldBe` False

      it "выставляет поле blackCanCastleQueenside в False" $
        blackCanCastleQueenside (disableCastling Black initialBoard) `shouldBe` False

      it "поле whiteCanCastleKingside без изменения" $
        whiteCanCastleKingside (disableCastling Black initialBoard) `shouldBe` True

      it "поле whiteCanCastleQueenside без изменения" $
        whiteCanCastleQueenside (disableCastling Black initialBoard) `shouldBe` True

  describe "move" $ do
    context "[белые]" $ do
      it "протестировать все возможные ходы" pending

      it "белая пешка с начальной позиции" $ do
        let sqs = Map.delete ('e', 2) $ Map.insert ('e', 4) pawnWhite $ squares initialBoard
        (move initialBoard $ Move pawnWhite ('e',2) ('e',4)) `shouldBe` (initialBoard { squares = sqs, enPassantTarget = Just ('e', 3) })

      it "после хода пешкой через поле устанавливается значение enPassantTarget" $ do
        enPassantTarget (move initialBoard $ Move pawnWhite ('a',2) ('a',4)) `shouldBe` (Just ('a', 3))

      it "ход королём меняет поля whiteCanCastleKingside и whiteCanCastleQueenside на False" $ do
        whiteCanCastleKingside (move initialBoard $ Move kingWhite ('e', 1) ('e', 2)) `shouldBe` False
        whiteCanCastleQueenside (move initialBoard $ Move kingWhite ('e', 1) ('e', 2)) `shouldBe` False

      it "ход королевской ладьёй меняет поле whiteCanCastleKingside на False" $
        whiteCanCastleKingside (move initialBoard $ Move rookWhite ('h', 1) ('h', 2)) `shouldBe` False

      it "ход королевской ладьёй не меняет поле whiteCanCastleQueenside на False" $
        whiteCanCastleQueenside (move initialBoard $ Move rookWhite ('h', 1) ('h', 2)) `shouldBe` True

      it "ход ферзевой ладьёй меняет поле whiteCanCastleQueenside на False" $
        whiteCanCastleQueenside (move initialBoard $ Move rookWhite ('a', 1) ('a', 2)) `shouldBe` False

      it "ход ферзевой ладьёй не меняет поле whiteCanCastleKingside на False" $
        whiteCanCastleKingside (move initialBoard $ Move rookWhite ('a', 1) ('a', 2)) `shouldBe` True

      context "[рокировка]" $ do
        it "короткая рокировка белых выставляет поле whiteCanCastleKingside в False" $
          whiteCanCastleKingside (move initialBoard $ KingsideCastling White) `shouldBe` False

        it "короткая рокировка белых выставляет поле whiteCanCastleQueenside в False" $
          whiteCanCastleQueenside (move initialBoard $ KingsideCastling White) `shouldBe` False

        it "длинная рокировка белых выставляет поле whiteCanCastleKingside в False" $
          whiteCanCastleKingside (move initialBoard $ QueensideCastling White) `shouldBe` False

        it "длинная рокировка белых выставляет поле whiteCanCastleQueenside в False" $
          whiteCanCastleQueenside (move initialBoard $ QueensideCastling White) `shouldBe` False

    context "[чёрные]" $ do
      it "протестировать все возможные ходы" pending
    
      it "ход королём меняет поля blackCanCastleKingside и blackCanCastleQueenside на False" $ do
        blackCanCastleKingside (move initialBoard $ Move kingBlack ('e', 8) ('d', 8)) `shouldBe` False
        blackCanCastleQueenside (move initialBoard $ Move kingBlack ('e', 8) ('d', 8)) `shouldBe` False

      it "ход королевской ладьёй меняет поле blackCanCastleKingside на False" $
        blackCanCastleKingside (move initialBoard $ Move rookBlack ('h', 8) ('g', 8)) `shouldBe` False

      it "ход королевской ладьёй не меняет поле blackCanCastleQueenside на False" $
        blackCanCastleQueenside (move initialBoard $ Move rookWhite ('h', 8) ('g', 8)) `shouldBe` True

      it "ход ферзевой ладьёй меняет поле blackCanCastleQueenside на False" $
        blackCanCastleQueenside (move initialBoard $ Move rookBlack ('a', 8) ('b', 8)) `shouldBe` False

      it "ход ферзевой ладьёй не меняет поле blackCanCastleKingside на False" $
        blackCanCastleKingside (move initialBoard $ Move rookBlack ('a', 8) ('b', 8)) `shouldBe` True

      context "[рокировка]" $ do
        it "короткая рокировка чёрных выставляет поле blackCanCastleKingside в False" $
          blackCanCastleKingside (move initialBoard $ KingsideCastling Black) `shouldBe` False

        it "короткая рокировка чёрных выставляет поле blackCanCastleQueenside в False" $
          blackCanCastleQueenside (move initialBoard $ KingsideCastling Black) `shouldBe` False

        it "длинная рокировка чёрных выставляет поле blackCanCastleKingside в False" $
          blackCanCastleKingside (move initialBoard $ QueensideCastling Black) `shouldBe` False

        it "длинная рокировка чёрных выставляет поле blackCanCastleQueenside в False" $
          blackCanCastleQueenside (move initialBoard $ QueensideCastling Black) `shouldBe` False