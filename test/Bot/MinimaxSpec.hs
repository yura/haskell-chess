module Bot.MinimaxSpec (spec) where

import Test.Hspec
import Fixtures

import Bot.Minimax
import Board
import Board.InitialPosition
import Format.FEN.Import ( fromFEN )
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
          maxValue whiteLadderMatesIn2 3 `shouldBe` (maxBound, Move (Piece Rook White) (read "e1") (read "g1"))

      context "[мат в три хода]" $ do
        it "находит линейный мат в два хода" $ do
          pendingWith "Slow test"
          -- Чёрные подыгрывают белым, совершая ходы, приводящие к мату [Move (Piece Rook White) (read "d1") (read "h1"),Move (Piece King Black) (read "g4") (read "h5"),Move (Piece Rook White) (read "d2") (read "g2"),Move (Piece King Black) (read "f5") (read "g4"),Move (Piece Rook White) (read "e1") (read "d1")]"
          -- [Move (Piece Rook White) (read "d1") (read "h1"),Move (Piece King Black) (read "g4") (read "h5"),Move (Piece Rook White) (read "d2") (read "g2"),Move (Piece King Black) (read "f5") (read "g4"),Move (Piece Rook White) (read "e1") (read "d1")]
          maxValue whiteLadderMatesIn3 5 `shouldBe` (maxBound, Move (Piece Rook White) (read "d2") (read "f2"))

      context "[мат один ход (github issue #1)]" $ do
        it "ход 49" $ do
          pendingWith "Не может найти мат в один, если находит до этого любой другой мат, например, в 3 хода"
          let board = fromFEN "8/7K/1Q6/8/k7/2p5/PPPP1PPP/1RB3NR w - - 0 49"
          maxValue board 3 `shouldBe` (maxBound, Move (Piece Pawn White) (read "b2") (read "b3"))

    context "[чёрные]" $ do
      context "[мат в один ход]" $ do
        it "возращает максимально возможное значение оценки и ход конём" $ do
          maxValue whiteMatedByKninghtIn1 1 `shouldBe` (maxBound, Move knightBlack (read "a3") (read "c2"))

      context "[мат в два хода]" $ do
        it "находит линейный мат в два хода" $ do
          maxValue blackLadderMatesIn2 3 `shouldBe` (maxBound, Move (Piece Rook Black) (read "e1") (read "g1"))

      context "[мат в три хода]" $ do
        it "находит линейный мат в два хода" $ do
          pendingWith "Slow test"
          maxValue blackLadderMatesIn3 5 `shouldBe` (maxBound, Move (Piece Rook Black) (read "d2") (read "f2"))

  describe "minValue" $ do
    context "[начальная позиция]" $ do
      it "минимально возмножное значение для хода" $ do
        pending
        0 `shouldBe` 0
