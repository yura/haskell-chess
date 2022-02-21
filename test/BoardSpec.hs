module BoardSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map
import           Board
import           Board.InitialPosition
import Board (Board(halfmoveClock))

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "генерирует пустую доску размером 0 клеток" $
      length (squares emptyBoard) `shouldBe` 0

    it "значение halfmoveClock равно 0" $ do
      halfmoveClock emptyBoard `shouldBe` 0

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

  describe "canCastleKingside" $ do
    it "возращает True, если можно рокироваться в короткую сторону" $ do
      canCastleKingside White initialBoard `shouldBe` True
      canCastleKingside Black initialBoard `shouldBe` True

    it "возращает False, если можно рокироваться в короткую сторону" $ do
      canCastleKingside White emptyBoard `shouldBe` False
      canCastleKingside Black emptyBoard `shouldBe` False

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

  describe "pawnsToEnPassantAt" $ do
    context "[белые]" $ do

      context "[взятие на a6]" $ do
        it "возращает Fasle, если на b5 нет белой пешки" $
          pawnsToEnPassantAt ('a', 6) White initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на b5" $
          pawnsToEnPassantAt ('a', 6) White (placePiece ('b', 5) pawnWhite initialBoard) `shouldBe` True

      context "[взятие на e6]" $ do
        it "возращает Fasle, если на d5 и на f5 нет белых пешек" $
          pawnsToEnPassantAt ('e', 6) White initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на d5" $
          pawnsToEnPassantAt ('e', 6) White (placePiece ('d', 5) pawnWhite initialBoard) `shouldBe` True 

        it "возращает True если белая пешка стоит на f5" $
          pawnsToEnPassantAt ('e', 6) White (placePiece ('f', 5) pawnWhite initialBoard) `shouldBe` True 

      context "[взятие на h6]" $ do
        it "возращает Fasle, если на g5 нет белой пешки" $
          pawnsToEnPassantAt ('h', 6) White initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на g5" $
          pawnsToEnPassantAt ('h', 6) White (placePiece ('g', 5) pawnWhite initialBoard) `shouldBe` True 

    context "[чёрные]" $ do
      context "[взятие на a3]" $ do
        it "возращает Fasle, если на b4 нет чёрной пешки" $
          pawnsToEnPassantAt ('a', 3) Black initialBoard `shouldBe` False

        it "возращает True если чёрная пешка стоит на b4" $
          pawnsToEnPassantAt ('a', 3) Black (placePiece ('b', 4) pawnBlack initialBoard) `shouldBe` True 

      context "[взятие на d3]" $ do
        it "возращает Fasle, если на c5 и на e5 нет чёрных пешек" $
          pawnsToEnPassantAt ('d', 3) Black initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на c4" $
          pawnsToEnPassantAt ('d', 3) Black (placePiece ('c', 4) pawnBlack initialBoard) `shouldBe` True 

        it "возращает True если белая пешка стоит на e4" $
          pawnsToEnPassantAt ('d', 3) Black (placePiece ('e', 4) pawnBlack initialBoard) `shouldBe` True 

      context "[взятие на h3]" $ do
        it "возращает Fasle, если на g4 нет белой пешки" $
          pawnsToEnPassantAt ('h', 3) Black initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на g4" $
          pawnsToEnPassantAt ('h', 3) Black (placePiece ('g', 4) pawnBlack initialBoard) `shouldBe` True 

  describe "move" $ do
    context "[белые]" $ do
      context "[взятие на проходе]" $ do
        let board = foldl move initialBoard
                [ Move pawnWhite ('e', 2) ('e', 4)
                , Move pawnBlack ('a', 7) ('a', 6)
                , Move pawnWhite ('e', 4) ('e', 5)
                , Move pawnBlack ('d', 7) ('d', 5)
                ]

        it "выставление enPassantTarget после хода через поле" $
          enPassantTarget board `shouldBe` Just ('d', 6)

        it "обнуляет enPassantTarget после взятия на проходе" $ do
          let b = move board (EnPassantCapture pawnWhite ('e', 5) ('d', 6))
          enPassantTarget b `shouldBe` Nothing

        it "обнуляет enPassantTarget после любого хода" $ do
          let b = move board (Move pawnWhite ('a', 2) ('a', 3))
          enPassantTarget b `shouldBe` Nothing

        it "обнуляет enPassantTarget после любого взятия" $ do
          let b = move board (Capture bishopWhite ('f', 1) ('a', 6))
          enPassantTarget b `shouldBe` Nothing

        it "высталяет enPassantTarget только если есть пешка, которая может сделать взятие на проходе" $ do
          let b = move board (Move pawnWhite ('a', 2) ('a', 4))
          enPassantTarget b `shouldBe` Nothing

      it "белая пешка с начальной позиции" $ do
        let sqs = Map.delete ('e', 2) $ Map.insert ('e', 4) pawnWhite $ squares initialBoard
        move initialBoard (Move pawnWhite ('e',2) ('e',4)) `shouldBe` (initialBoard { squares = sqs, enPassantTarget = Nothing })

      it "после хода пешкой через поле, значение enPassantTarget не устанавливается, если нет пешки противника готовой взять на проходе" $ do
        enPassantTarget (move initialBoard $ Move pawnWhite ('a',2) ('a',4)) `shouldBe` Nothing

      it "(белые) после хода пешкой через поле устанавливается значение enPassantTarget в случае, если есть пешка противника готовая взять на проходе" $ do
        let board = placePiece ('b', 4) pawnBlack initialBoard
        enPassantTarget (move board $ Move pawnWhite ('a',2) ('a',4)) `shouldBe` Just ('a', 3)

      it "(чёрные) после хода пешкой через поле устанавливается значение enPassantTarget в случае, если есть пешка противника готовая взять на проходе" $ do
        let board = placePiece ('b', 5) pawnWhite initialBoard
        enPassantTarget (move board $ Move pawnBlack ('c', 7) ('c', 5)) `shouldBe` Just ('c', 6)

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

      context "[halfmoveClock]" $ do
        it "ход королём увеличивает счётчик полуходов на 1" $ do
          halfmoveClock (move initialBoard $ Move kingWhite ('e', 1) ('e', 2)) `shouldBe` 1

        it "ход королевской ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookWhite ('h', 1) ('g', 1)) `shouldBe` 1

        it "ход ферзевой ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookWhite ('a', 1) ('b', 1)) `shouldBe` 1

      context "[рокировка]" $ do
        it "короткая рокировка белых выставляет поле whiteCanCastleKingside в False" $
          whiteCanCastleKingside (move initialBoard $ KingsideCastling White) `shouldBe` False

        it "короткая рокировка белых выставляет поле whiteCanCastleQueenside в False" $
          whiteCanCastleQueenside (move initialBoard $ KingsideCastling White) `shouldBe` False

        it "длинная рокировка белых выставляет поле whiteCanCastleKingside в False" $
          whiteCanCastleKingside (move initialBoard $ QueensideCastling White) `shouldBe` False

        it "длинная рокировка белых выставляет поле whiteCanCastleQueenside в False" $
          whiteCanCastleQueenside (move initialBoard $ QueensideCastling White) `shouldBe` False

        it "короткая рокировка увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ KingsideCastling Black) `shouldBe` 1

        it "длинная рокировка увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ QueensideCastling Black) `shouldBe` 1

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

      context "[halfmoveClock]" $ do
        it "ход королём увеличивает счётчик полуходов на 1" $ do
          halfmoveClock (move initialBoard $ Move kingBlack ('e', 8) ('e', 7)) `shouldBe` 1

        it "ход королевской ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookBlack ('h', 8) ('g', 8)) `shouldBe` 1

        it "ход ферзевой ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookBlack ('a', 8) ('b', 8)) `shouldBe` 1

      context "[рокировка]" $ do
        it "короткая рокировка чёрных выставляет поле blackCanCastleKingside в False" $
          blackCanCastleKingside (move initialBoard $ KingsideCastling Black) `shouldBe` False

        it "короткая рокировка чёрных выставляет поле blackCanCastleQueenside в False" $
          blackCanCastleQueenside (move initialBoard $ KingsideCastling Black) `shouldBe` False

        it "длинная рокировка чёрных выставляет поле blackCanCastleKingside в False" $
          blackCanCastleKingside (move initialBoard $ QueensideCastling Black) `shouldBe` False

        it "длинная рокировка чёрных выставляет поле blackCanCastleQueenside в False" $
          blackCanCastleQueenside (move initialBoard $ QueensideCastling Black) `shouldBe` False

        it "короткая рокировка увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ KingsideCastling Black) `shouldBe` 1

        it "длинная рокировка увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ QueensideCastling Black) `shouldBe` 1

