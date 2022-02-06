module LawsSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map

import           Board
import           Laws

spec :: Spec
spec = do
  describe "bottomLeft" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      bottomLeft ('d', 4) `shouldBe` [('c', 3), ('b', 2), ('a', 1)]

    it "возращает пустой список из 'a2'" $
      bottomLeft ('a', 2) `shouldBe` []

    it "возращает пустой список из 'b1'" $
      bottomLeft ('b', 1) `shouldBe` []

    it "возращает 'a7' из 'b8'" $
      bottomLeft ('b', 8) `shouldBe` [('a', 7)]

    it "возращает главную диагональ из 'h8'" $
      bottomLeft ('h', 8) `shouldBe` [('g', 7), ('f', 6), ('e', 5), ('d', 4), ('c', 3), ('b', 2), ('a', 1)]

    it "возращает пустой список из 'a1..a8' и 'a1..h1'" $ do
      let names = [(r, 1) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map (\n -> bottomLeft n) names
      nub result `shouldBe` [[]]

  describe "bottomRight" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      bottomRight ('d', 4) `shouldBe` [('e', 3), ('f', 2), ('g', 1)]

    it "возращает 'b1' из 'a2'" $
      bottomRight ('a', 2) `shouldBe` [('b', 1)]

    it "возращает пустой список из 'b1'" $
      bottomRight ('b', 1) `shouldBe` []

    it "возращает всю диагональ из 'b8'" $
      bottomRight ('b', 8) `shouldBe` [('c', 7), ('d', 6), ('e', 5), ('f', 4), ('g', 3), ('h', 2)]

    it "возращает пустой список из 'h8'" $
      bottomRight ('h', 8) `shouldBe` []

    it "возращает пустой список из 'a1..h1' и 'h1..h8'" $ do
      let names = [(r, 1) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map (\n -> bottomRight n) names
      nub result `shouldBe` [[]]

  describe "topRight" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      topRight ('d', 4) `shouldBe` [('e', 5), ('f', 6), ('g', 7), ('h', 8)]

    it "возращает всю диагональ из 'a2'" $
      topRight ('a', 2) `shouldBe` [('b', 3), ('c', 4), ('d', 5), ('e', 6), ('f', 7), ('g', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map (\n -> topRight n) names
      nub result `shouldBe` [[]]

  describe "topLeft" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      topLeft ('d', 4) `shouldBe` [('c', 5), ('b', 6), ('a', 7)]

    it "возращает всю диагональ из 'h4'" $
      topLeft ('h', 4) `shouldBe` [('g', 5), ('f', 6), ('e', 7), ('d', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map (\n -> topLeft n) names
      nub result `shouldBe` [[]]

  describe "bishopMoves" $ do
    it "все ходы слона, если слон находится в центре" $ do
      bishopMoves ('d', 4) `shouldBe` [ ('c', 3), ('b', 2), ('a', 1), ('e', 3), ('f', 2), ('g', 1), ('e', 5), ('f', 6), ('g', 7), ('h', 8), ('c', 5), ('b', 6), ('a', 7) ]

  describe "knightMoves" $ do
    it "все ходы коня, если конь находится в центре" $ do
      knightMoves ('d', 4) `shouldBe` [ ('b', 3), ('c', 2), ('e', 2), ('f', 3), ('f', 5), ('e', 6), ('c', 6), ('b', 5) ]

    it "оба хода коня, если конь находится на 'a1'" $ do
      knightMoves ('a', 1) `shouldBe` [ ('c', 2), ('b', 3) ]

    it "оба хода коня, если конь находится на 'a1'" $ do
      knightMoves ('h', 1) `shouldBe` [ ('g', 3), ('f', 2) ]

    it "оба хода коня, если конь находится на 'h8'" $ do
      knightMoves ('h', 8) `shouldBe` [ ('f', 7), ('g', 6) ]

    it "оба хода коня, если конь находится на 'a8'" $ do
      knightMoves ('a', 8) `shouldBe` [ ('b', 6), ('c', 7) ]

    it "три хода коня, если конь находится на 'b1'" $ do
      knightMoves ('b', 1) `shouldBe` [ ('d', 2), ('c', 3), ('a', 3) ]

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

  describe "whitePawnMoveSquares" $ do
    context "[без взятия фигур противника]" $ do
      it "со своей начальной позиции может ходить на поле вперед или на два поля вперед" $ do
        whitePawnMoveSquares ('a', 2) `shouldBe` [ ('a', 3), ('a', 4) ]
        whitePawnMoveSquares ('e', 2) `shouldBe` [ ('e', 3), ('e', 4) ]

      it "может ходить на поле вперёд не с начальной позиции" $ do
        whitePawnMoveSquares ('b', 3) `shouldBe` [ ('b', 4) ]
        whitePawnMoveSquares ('g', 7) `shouldBe` [ ('g', 8) ]

  describe "blackPawnMoves" $ do
    context "[без взятия фигур противника]" $ do
      it "со своей начальной позиции может ходить на поле вперед или на два поля вперед" $ do
        blackPawnMoves ('a', 7) `shouldBe` [ ('a', 6), ('a', 5) ]
        blackPawnMoves ('e', 7) `shouldBe` [ ('e', 6), ('e', 5) ]

      it "может ходить на поле вперёд не с начальной позиции" $ do
        blackPawnMoves ('b', 3) `shouldBe` [ ('b', 2) ]
        blackPawnMoves ('g', 2) `shouldBe` [ ('g', 1) ]

  describe "whitePawnPossibleMoves" $ do
    context "[пешка на начальной позиции]" $ do
      it "ходит вперед и через поле" $ do
        let board = placePiece ('e', 2) pawnWhite emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3), Move pawnWhite ('e', 2) ('e', 4)]

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит своя фигура" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 3), knightWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` []

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит фигура противника" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 3), knightBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` []

      it "не ходит через клетку, если поле занято своей фигурой" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 4), pawnWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3)]

      it "не ходит через клетку, если поле занято фигурой противника" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 4), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3)]

      it "не ходит, если король после хода оказывается под шахом" $ pending
      it "не ходит через поле, если король после хода оказывается под шахом" $ pending

    context "[пешка не в начальной позиции]" $ do
      it "ходит вперед" $ do
        let board = placePiece ('e', 4) pawnWhite emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Move pawnWhite ('e', 4) ('e', 5)]
        
      it "не ходит вперед, если стоит своя фигура" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('e', 5), knightWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` []

      it "не ходит вперед, если стоит фигура противника" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('e', 5), knightBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` []

      it "рубит фигуру противника" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('d', 5), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Capture pawnWhite ('e', 4) ('d', 5), Move pawnWhite ('e', 4) ('e', 5)]

      it "не рубит свою фигуру" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('d', 5), pawnWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Move pawnWhite ('e', 4) ('e', 5)]

      it "не ходит, если король после хода оказывается под шахом" $ pending
      it "не ходит через поле, если король после хода оказывается под шахом" $ pending

    context "взятие на проходе" $ do
      it "рубит пешку противника на проходе" $ do
        let board = move (placePiece ('e', 5) pawnWhite emptyBoard) (Move pawnBlack ('f', 7) ('f', 5)) 
        whitePawnPossibleMoves ('e', 5) board `shouldBe` [EnPassantCapture pawnWhite ('e', 5) ('f', 6), Move pawnWhite ('e', 5) ('e', 6)]

      it "не ходит, если король после хода оказывается под шахом" $ pending
      it "не ходит через поле, если король после хода оказывается под шахом" $ pending


    context "продвижение" $ do
      it "продвигается до ферзя, ладьи, слона или коня" $ pending
      it "может рубить фигуры противника с последующим продвижением" $ pending
      it "не может рубить свои фигуры с последующим продвижением" $ pending

      it "не ходит, если король после хода оказывается под шахом" $ pending
      it "не ходит через поле, если король после хода оказывается под шахом" $ pending

  describe "kingPossibleMoves" $ do
    context "[рокировка]" $ do
      it "не ходит через клетку вперед, если поле занято своей фигурой" $ pending
      it "не ходит через клетку вперед, если поле занято фигурой противника" $ pending

      it "может рубить фигуру противника" $ do
        let board = placePieces [(('e', 4), Piece Pawn White), (('d', 5), Piece Pawn Black)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Capture pawnWhite ('e', 4) ('d', 5), Move pawnWhite ('e', 4) ('e', 5)]

      it "не рубит свои фигуры" $ pending
        --let board = placePieces [(('e', 4), Piece Pawn White), (('d', 5), Piece Pawn Black)] emptyBoard
        --whitePawnPossibleMoves ('e', 4) board `shouldBe` [Capture pawnWhite ('e', 4) ('d', 5), Move pawnWhite ('e', 4) ('e', 5)]

      it "может рубить пешку противника на проходе" $ do
        let board = placePieces [(('e', 4), Piece Pawn White), (('d', 5), Piece Pawn Black)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Capture pawnWhite ('e', 4) ('d', 5), Move pawnWhite ('e', 4) ('e', 5)]

      it "не ходит, если король после хода оказывается под шахом" $ pending

  describe "kingPossibleMoves" $ do
    context "[рокировка]" $ do
      it "не ходит под шах" $ pending
      it "не встречается с королём (частный случай шаха)" $ pending
      it "не рокируется, если ходил" $ pending
      it "не рокируется, если ходила ладья?" $ pending

    it "не ходит под шах" $ pending

