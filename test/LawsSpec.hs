module LawsSpec (spec) where

import Test.Hspec

import           Data.Either ( fromRight )
import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map
import           Text.Parsec

import           Board
import           Board.InitialPosition
import           Laws
import qualified Format.PGN.Import as PGN
import           Fixtures 

spec :: Spec
spec = do
  describe "allCaptureThreatSquares" $ do
    context "[белая пешка]" $ do
      it "возращает список всех полей, которым угрожает белая пешка" $ do
        allCaptureThreatSquares White (placePiece (read "e6") pawnWhite initialBoard) `shouldBe` [(read "d7"), (read "f7")]

      it "возращает список всех полей, которым угрожают две белые пешки" $ do
        allCaptureThreatSquares White (placePieces [((read "e6"), pawnWhite), ((read "d6"), pawnWhite)] initialBoard) `shouldBe` [(read "c7"),(read "e7"),(read "d7"),(read "f7")]

    context "[чёрная пешка]" $ do
      it "возращает список всех полей, которым угрожает чёрная пешка" $ do
        allCaptureThreatSquares Black (placePiece (read "e3") pawnBlack initialBoard) `shouldBe` [(read "d2"), (read "f2")]

      it "возращает список всех полей, которым угрожают две черные пешки" $ do
        allCaptureThreatSquares Black (placePieces [((read "e3"), pawnBlack), ((read "d3"), pawnBlack)] initialBoard) `shouldBe` [(read "c2"),(read "e2"),(read "d2"),(read "f2")]

    context "[конь]" $ do
      it "возращает список полей, которым угрожает конь" $ do
        allCaptureThreatSquares White (placePiece (read "e6") knightWhite initialBoard) `shouldBe` map read ["c7", "d8", "f8", "g7"]
        allCaptureThreatSquares Black (placePiece (read "e2") knightBlack initialBoard) `shouldBe` [(read "c1"), (read "g1")]

    context "[слон]" $ do
      it "возращает список полей, которым угрожает слон" $ do
        allCaptureThreatSquares White (placePieces [((read "e4"), bishopWhite), ((read "b7"), pawnBlack), ((read "h7"), pawnBlack)] emptyBoard) `shouldBe`
          [(read "h7"), (read "b7")]
        allCaptureThreatSquares Black (placePieces [((read "e4"), bishopBlack), ((read "c2"), pawnWhite), ((read "g2"), pawnWhite)] emptyBoard) `shouldBe`
          [(read "c2"), (read "g2")]

    context "[ладья]" $ do
      it "возращает список полей, которым угрожает ладья" $ do
        allCaptureThreatSquares White (placePieces [((read "e4"), rookWhite), ((read "e7"), pawnBlack), ((read "a4"), pawnBlack)] emptyBoard) `shouldBe`
          [(read "a4"), (read "e7")]
        allCaptureThreatSquares Black (placePieces [((read "d5"), rookBlack), ((read "h5"), pawnWhite), ((read "d2"), pawnWhite)] emptyBoard) `shouldBe`
          [(read "d2"), (read "h5")]

    context "[ферзь]" $ do
      it "возращает список полей, которым угрожает ферзь" $ do
        allCaptureThreatSquares White (placePiece (read "d4") queenWhite initialBoard) `shouldBe`
          [(read "g7"), (read "a7"), (read "d7")]
        allCaptureThreatSquares Black (placePieces [((read "d5"), rookBlack), ((read "h5"), pawnWhite), ((read "d2"), pawnWhite)] emptyBoard) `shouldBe`
          [(read "d2"), (read "h5")]

    context "[король]" $ do
      it "возращает список полей, которым угрожает король" $ do
        let board = placePieces
              [ ((read "e6"), kingWhite)
              , ((read "d7"), pawnBlack)
              , ((read "e7"), pawnBlack)
              , ((read "f7"), pawnBlack)] emptyBoard
        allCaptureThreatSquares White board `shouldBe` [(read "f7"), (read "d7"), (read "e7")]

      it "не угрожает фигурам, которые находятся под защитой ферзя" $ do
        let board = placePieces
              [ ((read "e6"), kingWhite)
              , ((read "d7"), pawnBlack)
              , ((read "e7"), pawnBlack)
              , ((read "f7"), pawnBlack)
              , ((read "e8"), queenBlack)] emptyBoard
        allCaptureThreatSquares White board `shouldBe` []

      it "не угрожает фигурам, которые находятся под защитой короля" $ do
        let board = placePieces
              [ ((read "e6"), kingWhite)
              , ((read "d7"), pawnBlack)
              , ((read "e7"), pawnBlack)
              , ((read "f7"), pawnBlack)
              , ((read "e8"), kingBlack)] emptyBoard
        allCaptureThreatSquares White board `shouldBe` []

  describe "isCheck" $ do
    it "возращает False в начальной позиции" $ do
      isCheck White initialBoard `shouldBe` False
      isCheck Black initialBoard { nextMove = Black } `shouldBe` False

    context "[пешка]" $ do
      it "возращает False, если пешка не шахует короля" $ do
        isCheck Black (placePieces [((read "e7"), pawnWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

      it "возращает True, если пешка шахует короля" $ do
        isCheck Black (placePieces [((read "d7"), pawnWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True

    context "[конь]" $ do
      it "возращает True, если конь угрожает королю противника" $ do
        isCheck Black (placePieces [((read "d6"), knightWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True

      it "возращает True, если короля 'прикрывает' фигура противника" $ do
        isCheck Black (placePiece (read "d6") knightWhite initialBoard { nextMove = Black }) `shouldBe` True

      it "возращает False, если конь может сходить на поле своего же короля" $ do
        isCheck Black (placePieces [((read "d6"), knightBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

    context "[слон]" $ do
      it "возращает True, если слон угрожает королю противника" $ do
        isCheck Black (placePieces [((read "a4"), bishopWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [((read "a4"), bishopWhite), ((read "d7"), pawnBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

      it "возращает False, если свой слон может сходить на поле короля" $ do
        isCheck Black (placePieces [((read "a4"), bishopBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

    context "[ладья]" $ do
      it "возращает True, если ладья угрожает королю противника" $ do
        isCheck Black (placePieces [((read "e1"), rookWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True
        isCheck Black (placePieces [((read "a8"), rookWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [((read "e1"), rookWhite), ((read "e7"), pawnBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

      it "возращает False, если своя ладья может сходить на поле короля" $ do
        isCheck Black (placePieces [((read "e1"), rookBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

    context "[ферзь]" $ do
      it "возращает True, если ферзь угрожает королю противника" $ do
        isCheck Black (placePieces [((read "e1"), queenWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True
        isCheck Black (placePieces [((read "a8"), queenWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True
        isCheck Black (placePieces [((read "b5"), queenWhite), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` True

      it "возращает False, если короля прикрывает фигура противника" $ do
        isCheck Black (placePieces [((read "e1"), queenWhite), ((read "e7"), pawnBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False
        isCheck Black (placePieces [((read "b5"), queenWhite), ((read "d7"), pawnBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

      it "возращает False, если своя ладья может сходить на поле короля" $ do
        isCheck Black (placePieces [((read "e1"), rookBlack), ((read "e8"), kingBlack)] emptyBoard { nextMove = Black }) `shouldBe` False

     -- Король не может шаховать

  describe "isDeadPosition" $ do
    it "возращает True, если на доске два короля" $ do
      let board = placePieces [((read "e4"), kingWhite), ((read "a8"), kingBlack)] emptyBoard
      isDeadPosition board `shouldBe` True
      isDeadPosition board `shouldBe` True

    it "возращает True, если на доске два короля и слон https://lichess.org/editor/8/2b2k2/8/8/6K1/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [((read "g4"), kingWhite), ((read "f7"), kingBlack), ((read "c7"), bishopBlack)] emptyBoard
      isDeadPosition board `shouldBe` True
  
  describe "isFiftyMove" $ do
    it "возвращает False для начальной позиции" $
      isFiftyMove initialBoard `shouldBe` False

    it "возвращает True если совершено 99 ходов без хода пешкой или взятий" $ do
      content <- readFile "test/fixtures/draw-50-moves.pgn"
      let board = PGN.toBoard content
      isFiftyMove board `shouldBe` True

    it "возвращает False, если не хватает хода" $ do
      content <- readFile "test/fixtures/almost-draw-50-moves.pgn"
      let board = PGN.toBoard content
      isFiftyMove board `shouldBe` False

  describe "isThreefoldRepetition" $ do
    it "возвращает False для начальной позиции" $
      isThreefoldRepetition initialBoard `shouldBe` False

    it "возвращает True при троекратном повторении позиции" $ do
      let board = placePieces [((read "a1"), queenWhite), ((read "b1"), kingWhite), ((read "h1"), kingBlack)] emptyBoard
      let moves =
            [ Move kingWhite (read "b1") (read "c1") -- 1st
            , Move kingBlack (read "h1") (read "g1")
            , Move kingWhite (read "c1") (read "b1")
            , Move kingBlack (read "g1") (read "h1")
            , Move kingWhite (read "b1") (read "c1") -- 2nd
            , Move kingBlack (read "h1") (read "g1")
            , Move kingWhite (read "c1") (read "b1")
            , Move kingBlack (read "g1") (read "h1")
            , Move kingWhite (read "b1") (read "c1") -- 3rd
            ]
                
      isThreefoldRepetition (applyMoves board moves) `shouldBe` True

  describe "isStalemate" $ do
    it "возвращает True, в случае пата https://lichess.org/editor/7k/5K2/6Q1/8/8/8/8/8_b_-_-_0_1" $ do
      let board = placePieces [((read "f7"), kingWhite), ((read "g6"), queenWhite), ((read "h8"), kingBlack)] emptyBoard  { nextMove = Black }
      isStalemate board `shouldBe` True

    it "возвращает False, в случае мата https://lichess.org/editor/7k/5KQ1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [((read "f7"), kingWhite), ((read "g7"), queenWhite), ((read "h8"), kingBlack)] emptyBoard { nextMove = Black }
      isStalemate board `shouldBe` False

    it "возвращает False, в случае шаха https://lichess.org/editor/7k/4K1Q1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [((read "e7"), kingWhite), ((read "g7"), queenWhite), ((read "h8"), kingBlack)] emptyBoard { nextMove = Black }
      isStalemate board `shouldBe` False

  describe "isMate" $ do
    it "возвращает False, в случае пата https://lichess.org/editor/7k/5K2/6Q1/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [((read "f7"), kingWhite), ((read "g6"), queenWhite), ((read "h8"), kingBlack)] emptyBoard
      isMate board { nextMove = Black } `shouldBe` False

    it "возвращает True, в случае мата https://lichess.org/editor/7k/5KQ1/8/8/8/8/8/8_b_-_-_0_1" $ do
      let board = placePieces [((read "f7"), kingWhite), ((read "g7"), queenWhite), ((read "h8"), kingBlack)] emptyBoard { nextMove = Black }
      isMate board  `shouldBe` True

    it "возвращает True для всех матов чёрным" $ do
      map isMate blackMated `shouldBe` [True, True]

    it "возвращает True для всех матов белым" $ do
      map isMate whiteMated `shouldBe` [True, True, True]

    it "возвращает False, в случае шаха https://lichess.org/editor/7k/4K1Q1/8/8/8/8/8/8_w_-_-_0_1" $ do
      let board = placePieces [((read "e7"), kingWhite), ((read "g7"), queenWhite), ((read "h8"), kingBlack)] emptyBoard
      isMate board { nextMove = Black } `shouldBe` False
