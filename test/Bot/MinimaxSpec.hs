module Bot.MinimaxSpec (spec) where

import Test.Hspec
import Fixtures

import Bot.Minimax
import Board
import Board.InitialPosition
import Laws

spec :: Spec
spec = do
  describe "maxValue" $ do
    context "[начальная позиция]" $ do
      it "возращает 0 как максимальное возможное значение для хода для глубины 1" $ do
        fst (maxValue initialBoard 1) `shouldBe` 0

      it "возращает 0 как максимальное возможное значение для хода для глубины 2" $ do
        fst (maxValue initialBoard 1) `shouldBe` 0

    context "[белые]" $ do
      context "[мат в один ход]" $ do
        it "возращает максимально возможное значение как максимальное возможное значение для хода для глубины 1" $ do
          fst (maxValue blackMatedByQueenIn1 1) `shouldBe` maxBound

      context "[мат в два хода]" $ do
        it "находит линейный мат в два хода" $ do
          maxValue whiteLadderMatesIn2 3 `shouldBe` (maxBound, Move (Piece Rook White) ('e',1) ('g',1))

      context "[мат в три хода]" $ do
        it "находит линейный мат в два хода" $ do
          pendingWith "Slow test"
          -- Чёрные подыгрывают белым, совершая ходы, приводящие к мату [Move (Piece Rook White) ('d',1) ('h',1),Move (Piece King Black) ('g',4) ('h',5),Move (Piece Rook White) ('d',2) ('g',2),Move (Piece King Black) ('f',5) ('g',4),Move (Piece Rook White) ('e',1) ('d',1)]"
          -- [Move (Piece Rook White) ('d',1) ('h',1),Move (Piece King Black) ('g',4) ('h',5),Move (Piece Rook White) ('d',2) ('g',2),Move (Piece King Black) ('f',5) ('g',4),Move (Piece Rook White) ('e',1) ('d',1)]
          maxValue whiteLadderMatesIn3 5 `shouldBe` (maxBound, Move (Piece Rook White) ('d',2) ('f',2))

    context "[чёрные]" $ do
      context "[мат в один ход]" $ do
        it "возращает максимально возможное значение оценки и ход конём" $ do
          maxValue whiteMatedByKninghtIn1 1 `shouldBe` (maxBound, Move knightBlack ('a', 3) ('c', 2))

      context "[мат в два хода]" $ do
        it "находит линейный мат в два хода" $ do
          maxValue blackLadderMatesIn2 3 `shouldBe` (maxBound, Move (Piece Rook Black) ('e',1) ('g',1))

      context "[мат в три хода]" $ do
        it "находит линейный мат в два хода" $ do
          pendingWith "Slow test"
          maxValue blackLadderMatesIn3 5 `shouldBe` (maxBound, Move (Piece Rook Black) ('d',2) ('f',2))

  describe "minValue" $ do
    context "[начальная позиция]" $ do
      it "минимально возмножное значение для хода" $ do
        pending
        0 `shouldBe` 0

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
