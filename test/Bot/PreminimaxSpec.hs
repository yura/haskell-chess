module Bot.PreminimaxSpec (spec) where

import Test.Hspec
import Fixtures

import Bot.Preminimax
import Board
import Board.InitialPosition
import Laws

spec :: Spec
spec = do
  describe "findWinningMove" $ do
    it "находит мат в один ход" $  do
      let board = placePieces
            [ (('f', 1), rookWhite)
            , (('h', 8), kingBlack)
            , (('h', 7), pawnBlack)
            , (('g', 7), pawnBlack)
            , (('g', 1), kingWhite)] emptyBoard
      let moves = possibleMoves board
      findWinningMove board moves `shouldBe` Just (Move rookWhite ('f', 1) ('f', 8))

    it "находит мат за чёрных" $  do
      let board = placePieces
            [ (('f', 3), kingBlack)
            , (('b', 3), queenBlack)
            , (('f', 1), kingWhite)] emptyBoard { nextMove = Black }
      let moves = possibleMoves board
      findWinningMove board moves `shouldBe` Just (Move queenBlack ('b', 3) ('d', 1))

    it "возращает Nothing, если нет мата в один ход" $  do
      let board = placePieces
            [ (('f', 8), kingBlack)
            , (('b', 3), queenBlack)
            , (('f', 6), kingWhite)] emptyBoard { nextMove = Black }
      let moves = possibleMoves board
      findWinningMove board moves `shouldBe` Nothing

  describe "eliminateLosingMoves" $ do
    it "находит ход, спасающий от мата в один ход" $  do
      let board = placePieces
            [ (('f', 1), rookWhite)
            , (('h', 8), kingBlack)
            , (('h', 7), pawnBlack)
            , (('g', 7), pawnBlack)
            , (('g', 1), kingWhite)] emptyBoard
      eliminateLosingMoves (board { nextMove = Black }) `shouldBe`
        [ Move (Piece Pawn Black) ('h',7) ('h',6)
        , Move (Piece Pawn Black) ('h',7) ('h',5)
        , Move (Piece Pawn Black) ('g',7) ('g',6)
        , Move (Piece Pawn Black) ('g',7) ('g',5)
        , Move (Piece King Black) ('h',8) ('g',8)
        ]

  describe "findTwoStepWin" $ do
    it "находит ход, спасающий от мата в один ход" $  do
      let board = placePieces
            [ (('a', 8), queenBlack)
            , (('h', 8), kingBlack)
            , (('g', 7), pawnBlack)
            , (('h', 7), pawnBlack)
            , (('f', 2), queenWhite)
            , (('f', 1), rookWhite)
            , (('g', 1), kingWhite)] emptyBoard
      findTwoStepWin board `shouldBe` Just (Move queenWhite ('f', 2) ('f', 8))
