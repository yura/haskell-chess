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
