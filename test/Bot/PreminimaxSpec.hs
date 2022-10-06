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
            [ ((read "f1"), rookWhite)
            , ((read "h8"), kingBlack)
            , ((read "h7"), pawnBlack)
            , ((read "g7"), pawnBlack)
            , ((read "g1"), kingWhite)] emptyBoard
      let moves = possibleMoves board
      findWinningMove board moves `shouldBe` Just (Move rookWhite (read "f1") (read "f8"))

    it "находит мат за чёрных" $  do
      let board = placePieces
            [ ((read "f3"), kingBlack)
            , ((read "b3"), queenBlack)
            , ((read "f1"), kingWhite)] emptyBoard { nextMove = Black }
      let moves = possibleMoves board
      findWinningMove board moves `shouldBe` Just (Move queenBlack (read "b3") (read "d1"))

    it "возращает Nothing, если нет мата в один ход" $  do
      let board = placePieces
            [ ((read "f8"), kingBlack)
            , ((read "b3"), queenBlack)
            , ((read "f6"), kingWhite)] emptyBoard { nextMove = Black }
      let moves = possibleMoves board
      findWinningMove board moves `shouldBe` Nothing

  describe "eliminateLosingMoves" $ do
    it "находит ход, спасающий от мата в один ход" $  do
      let board = placePieces
            [ ((read "f1"), rookWhite)
            , ((read "h8"), kingBlack)
            , ((read "h7"), pawnBlack)
            , ((read "g7"), pawnBlack)
            , ((read "g1"), kingWhite)] emptyBoard
      eliminateLosingMoves (board { nextMove = Black }) `shouldBe`
        [ Move (Piece King Black) (read "h8") (read "g8")
        , Move (Piece Pawn Black) (read "h7") (read "h6")
        , Move (Piece Pawn Black) (read "h7") (read "h5")
        , Move (Piece Pawn Black) (read "g7") (read "g6")
        , Move (Piece Pawn Black) (read "g7") (read "g5")
        ]

  describe "findTwoStepWin" $ do
    it "находит ход, спасающий от мата в один ход" $  do
      let board = placePieces
            [ ((read "a8"), queenBlack)
            , ((read "h8"), kingBlack)
            , ((read "g7"), pawnBlack)
            , ((read "h7"), pawnBlack)
            , ((read "f2"), queenWhite)
            , ((read "f1"), rookWhite)
            , ((read "g1"), kingWhite)] emptyBoard
      findTwoStepWin board `shouldBe` Just (Move queenWhite (read "f2") (read "f8"))
