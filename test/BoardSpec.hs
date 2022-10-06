module BoardSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map
import           Board
import           Board.InitialPosition
import qualified Format.PGN.Import as PGN

spec :: Spec
spec = do
  describe "emptyBoard" $ do
    it "генерирует пустую доску размером 64 клеток" $ do
      length (squares emptyBoard) `shouldBe` 64
      nub (squares emptyBoard) `shouldBe` [Nothing]

    it "значение halfmoveClock равно 0" $
      halfmoveClock emptyBoard `shouldBe` 0

  describe "squareFile" $ do
    it "возвращает 0 для колонки 'a'" $ do
      let ss = map Square $ take 8 [0, 8..]
      nub (map squareFile ss) `shouldBe` [0]

    it "возвращает 1 для колонки 'b'" $ do
      let ss = map Square $ take 8 [1, 9..]
      nub (map squareFile ss) `shouldBe` [1]

    it "возвращает 2 для колонки 'c'" $ do
      let ss = map Square $ take 8 [2, 10..]
      nub (map squareFile ss) `shouldBe` [2]

    it "возвращает 3 для колонки 'd'" $ do
      let ss = map Square $ take 8 [3, 11..]
      nub (map squareFile ss) `shouldBe` [3]

    it "возвращает 4 для колонки 'e'" $ do
      let ss = map Square $ take 8 [4, 12..]
      nub (map squareFile ss) `shouldBe` [4]

    it "возвращает 5 для колонки 'f'" $ do
      let ss = map Square $ take 8 [5, 13..]
      nub (map squareFile ss) `shouldBe` [5]

    it "возвращает 6 для колонки 'g'" $ do
      let ss = map Square $ take 8 [6, 14..]
      nub (map squareFile ss) `shouldBe` [6]

    it "возвращает 7 для колонки 'h'" $ do
      let ss = map Square $ take 8 [7, 15..]
      nub (map squareFile ss) `shouldBe` [7]

  describe "squareRank" $ do
    it "возвращает 0 для ряда '1'" $ do
      let ss = map Square $ take 8 [0..]
      nub (map squareRank ss) `shouldBe` [0]

    it "возвращает 1 для ряда '2'" $ do
      let ss = map Square $ take 8 [8..]
      nub (map squareRank ss) `shouldBe` [1]

    it "возвращает 2 для ряда '3'" $ do
      let ss = map Square $ take 8 [16..]
      nub (map squareRank ss) `shouldBe` [2]

    it "возвращает 3 для ряда '4'" $ do
      let ss = map Square $ take 8 [24..]
      nub (map squareRank ss) `shouldBe` [3]

    it "возвращает 4 для ряда '5'" $ do
      let ss = map Square $ take 8 [32..]
      nub (map squareRank ss) `shouldBe` [4]

    it "возвращает 5 для ряда '6'" $ do
      let ss = map Square $ take 8 [40..]
      nub (map squareRank ss) `shouldBe` [5]

    it "возвращает 6 для ряда '7'" $ do
      let ss = map Square $ take 8 [48..]
      nub (map squareRank ss) `shouldBe` [6]

    it "возвращает 7 для ряда '8'" $ do
      let ss = map Square $ take 8 [56..]
      nub (map squareRank ss) `shouldBe` [7]

  describe "takenBy" $ do
    context "[поле не занято ни белыми, ни чёрными]" $ do
      it "возращает False, при запросе занято ли белыми" $ do
        takenBy White emptyBoard (read "e2") `shouldBe` False

      it "возращает False, при запросе занято ли чёрными" $ do
        takenBy Black emptyBoard (read "e2") `shouldBe` False

    context "[поле занято белыми]" $ do
      it "возращает True, при запросе занято ли белыми" $ do
        takenBy White initialBoard (read "e2") `shouldBe` True

      it "возращает False, при запросе занято ли чёрными" $ do
        takenBy Black initialBoard (read "e2") `shouldBe` False

    context "[поле занято чёрными]" $ do
      it "возращает False, при запросе занято ли белыми" $ do
        takenBy White initialBoard (read "e7") `shouldBe` False

      it "возращает True, при запросе занято ли чёрными" $ do
        takenBy Black initialBoard (read "e7") `shouldBe` True

  describe "pawnSquares" $ do
    it "возращает пустой список, если доска пустая белых пешек" $
      pawnSquares White emptyBoard `shouldBe` []

    it "возращает пустой список, если на доске не пешек данного цвета" $ do
      let board = placePiece (read "e2") pawnWhite emptyBoard
      pawnSquares Black board `shouldBe` []

    it "возращает все поля занятые пешками данного цвета" $ do
      let board = placePieces [((read "d2"), pawnWhite), ((read "e2"), pawnWhite)] emptyBoard
      map show (pawnSquares White board) `shouldBe` ["d2", "e2"]

  describe "placePiece" $ do
    it "ставит фигуру на заданную клетку" $ do
      let sqs = squares $ placePiece (read "d4") pawnWhite emptyBoard
      (sqs !! 27) `shouldBe` Just pawnWhite

  describe "placePieces" $ do
    it "ставит фигуры на заданные клетки" $ do
      let sqs = squares $ placePieces [((read "a2"), pawnWhite), ((read "a7"), pawnBlack)] emptyBoard
      (sqs !! 8) `shouldBe` Just pawnWhite
      (sqs !! 48) `shouldBe` Just pawnBlack

  describe "kingAt" $ do
    it "возращает позицию короля" $ do
      kingAt White initialBoard `shouldBe` Square 4
      kingAt Black initialBoard `shouldBe` Square 60

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
          pawnsToEnPassantAt (read "a6") initialBoard `shouldBe` False

        it "возращает False, если белая пешка стоит на c5" $
          pawnsToEnPassantAt (read "a6") (placePiece (read "c5") pawnWhite initialBoard) `shouldBe` False

        it "возращает True если белая пешка стоит на b5" $
          pawnsToEnPassantAt (read "a6") (placePiece (read "b5") pawnWhite initialBoard) `shouldBe` True

      context "[взятие на e6]" $ do
        it "возращает Fasle, если на d5 и на f5 нет белых пешек" $
          pawnsToEnPassantAt (read "e6") initialBoard `shouldBe` False

        it "возращает False, если белая пешка стоит на c5" $
          pawnsToEnPassantAt (read "e6") (placePiece (read "c5") pawnWhite initialBoard) `shouldBe` False

        it "возращает False, если белая пешка стоит на g5" $
          pawnsToEnPassantAt (read "e6") (placePiece (read "g5") pawnWhite initialBoard) `shouldBe` False

        it "возращает True, если белая пешка стоит на d5" $
          pawnsToEnPassantAt (read "e6") (placePiece (read "d5") pawnWhite initialBoard) `shouldBe` True

        it "возращает True если белая пешка стоит на f5" $
          pawnsToEnPassantAt (read "e6") (placePiece (read "f5") pawnWhite initialBoard) `shouldBe` True

      context "[взятие на h6]" $ do
        it "возращает Fasle, если на g5 нет белой пешки" $
          pawnsToEnPassantAt (read "h6") initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на g5" $
          pawnsToEnPassantAt (read "h6") (placePiece (read "g5") pawnWhite initialBoard) `shouldBe` True

    context "[чёрные]" $ do
      context "[взятие на a3]" $ do
        it "возращает Fasle, если на b4 нет чёрной пешки" $
          pawnsToEnPassantAt (read "a3") initialBoard `shouldBe` False

        it "возращает True если чёрная пешка стоит на b4" $
          pawnsToEnPassantAt (read "a3") (placePiece (read "b4") pawnBlack initialBoard) `shouldBe` True

      context "[взятие на d3]" $ do
        it "возращает Fasle, если на c5 и на e5 нет чёрных пешек" $
          pawnsToEnPassantAt (read "d3") initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на c4" $
          pawnsToEnPassantAt (read "d3") (placePiece (read "c4") pawnBlack initialBoard) `shouldBe` True

        it "возращает True если белая пешка стоит на e4" $
          pawnsToEnPassantAt (read "d3") (placePiece (read "e4") pawnBlack initialBoard) `shouldBe` True

      context "[взятие на h3]" $ do
        it "возращает Fasle, если на g4 нет белой пешки" $
          pawnsToEnPassantAt (read "h3") initialBoard `shouldBe` False

        it "возращает True если белая пешка стоит на g4" $
          pawnsToEnPassantAt (read "h3") (placePiece (read "g4") pawnBlack initialBoard) `shouldBe` True

  describe "move" $ do
    context "[белые]" $ do
      context "[взятие на проходе]" $ do
        let board = foldl move initialBoard
                [ Move pawnWhite (read "e2") (read "e4")
                , Move pawnBlack (read "a7") (read "a6")
                , Move pawnWhite (read "e4") (read "e5")
                , Move pawnBlack (read "d7") (read "d5")
                ]

        it "выставление enPassantTarget после хода через поле" $
          enPassantTarget board `shouldBe` Just (read "d6", read "d5")

        it "обнуляет enPassantTarget после взятия на проходе" $ do
          let b = move board (EnPassantCapture pawnWhite (read "e5") (read "d6"))
          enPassantTarget b `shouldBe` Nothing

        it "обнуляет enPassantTarget после любого хода" $ do
          let b = move board (Move pawnWhite (read "a2") (read "a3"))
          enPassantTarget b `shouldBe` Nothing

        it "обнуляет enPassantTarget после любого взятия" $ do
          let b = move board (Capture bishopWhite (read "f1") (read "a6"))
          enPassantTarget b `shouldBe` Nothing

        it "высталяет enPassantTarget только если есть пешка, которая может сделать взятие на проходе" $ do
          let b = move board (Move pawnWhite (read "a2") (read "a4"))
          enPassantTarget b `shouldBe` Nothing

      it "белая пешка с начальной позиции" $ do
        let sqs = insertAt (read "e4") pawnWhite $ deleteAt (read "e2") $ squares initialBoard
        move initialBoard (Move pawnWhite (read "e2") (read "e4")) `shouldBe` (initialBoard { squares = sqs, enPassantTarget = Nothing, nextMove = Black, history = [Move (Piece Pawn White) (read "e2") (read "e4")], fens = Map.fromList [("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq -", 1)] })

      it "после хода пешкой через поле, значение enPassantTarget не устанавливается, если нет пешки противника готовой взять на проходе" $ do
        enPassantTarget (move initialBoard $ Move pawnWhite (read "a2") (read "a4")) `shouldBe` Nothing

      it "(белые) после хода пешкой через поле устанавливается значение enPassantTarget в случае, если есть пешка противника готовая взять на проходе" $ do
        let board = placePiece (read "b4") pawnBlack initialBoard
        enPassantTarget (move board $ Move pawnWhite (read "a2") (read "a4")) `shouldBe` Just (read "a3", read "a4")

      it "(чёрные) после хода пешкой через поле устанавливается значение enPassantTarget в случае, если есть пешка противника готовая взять на проходе" $ do
        let board = placePiece (read "b5") pawnWhite initialBoard
        enPassantTarget (move board $ Move pawnBlack (read "c7") (read "c5")) `shouldBe` Just (read "c6", read "c5")

      it "ход королём меняет поля whiteCanCastleKingside и whiteCanCastleQueenside на False" $ do
        whiteCanCastleKingside (move initialBoard $ Move kingWhite (read "e1") (read "e2")) `shouldBe` False
        whiteCanCastleQueenside (move initialBoard $ Move kingWhite (read "e1") (read "e2")) `shouldBe` False

      it "ход королевской ладьёй меняет поле whiteCanCastleKingside на False" $
        whiteCanCastleKingside (move initialBoard $ Move rookWhite (read "h1") (read "h2")) `shouldBe` False

      it "ход королевской ладьёй не меняет поле whiteCanCastleQueenside на False" $
        whiteCanCastleQueenside (move initialBoard $ Move rookWhite (read "h1") (read "h2")) `shouldBe` True

      it "ход ферзевой ладьёй меняет поле whiteCanCastleQueenside на False" $
        whiteCanCastleQueenside (move initialBoard $ Move rookWhite (read "a1") (read "a2")) `shouldBe` False

      it "ход ферзевой ладьёй не меняет поле whiteCanCastleKingside на False" $
        whiteCanCastleKingside (move initialBoard $ Move rookWhite (read "a1") (read "a2")) `shouldBe` True

      context "[halfmoveClock]" $ do
        it "ход королём увеличивает счётчик полуходов на 1" $ do
          halfmoveClock (move initialBoard $ Move kingWhite (read "e1") (read "e2")) `shouldBe` 1

        it "ход королевской ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookWhite (read "h1") (read "g1")) `shouldBe` 1

        it "ход ферзевой ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookWhite (read "a1") (read "b1")) `shouldBe` 1

      context "[fens]" $ do
        it "повторение позиции увеличивает счётчик на 1" $ do
          let board = placePieces [((read "a1"), queenWhite), ((read "b1"), kingWhite), ((read "h1"), kingBlack)] emptyBoard
          let moves =
                [ Move kingWhite (read "b1") (read "c1") -- 1st
                , Move kingBlack (read "h1") (read "g1")
                , Move kingWhite (read "c1") (read "b1")
                , Move kingBlack (read "g1") (read "h1")
                , Move kingWhite (read "b1") (read "c1") -- 2nd
                ]
          -- https://lichess.org/analysis/fromPosition/8/8/8/8/8/8/8/Q1K4k_b_-_-_0_1
          M.lookup "8/8/8/8/8/8/8/Q1K4k b - -" (fens (applyMoves board moves)) `shouldBe` Just 2

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
        blackCanCastleKingside (move initialBoard $ Move kingBlack (read "e8") (read "d8")) `shouldBe` False
        blackCanCastleQueenside (move initialBoard $ Move kingBlack (read "e8") (read "d8")) `shouldBe` False

      it "ход королевской ладьёй меняет поле blackCanCastleKingside на False" $
        blackCanCastleKingside (move initialBoard $ Move rookBlack (read "h8") (read "g8")) `shouldBe` False

      it "ход королевской ладьёй не меняет поле blackCanCastleQueenside на False" $
        blackCanCastleQueenside (move initialBoard $ Move rookWhite (read "h8") (read "g8")) `shouldBe` True

      it "ход ферзевой ладьёй меняет поле blackCanCastleQueenside на False" $
        blackCanCastleQueenside (move initialBoard $ Move rookBlack (read "a8") (read "b8")) `shouldBe` False

      it "ход ферзевой ладьёй не меняет поле blackCanCastleKingside на False" $
        blackCanCastleKingside (move initialBoard $ Move rookBlack (read "a8") (read "b8")) `shouldBe` True

      context "[halfmoveClock]" $ do
        it "ход королём увеличивает счётчик полуходов на 1" $ do
          halfmoveClock (move initialBoard $ Move kingBlack (read "e8") (read "e7")) `shouldBe` 1

        it "ход королевской ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookBlack (read "h8") (read "g8")) `shouldBe` 1

        it "ход ферзевой ладьёй увеличивает счётчик полуходов на 1" $
          halfmoveClock (move initialBoard $ Move rookBlack (read "a8") (read "b8")) `shouldBe` 1

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

