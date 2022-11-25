module Bot.MinimaxSpec (spec) where

import Test.Hspec
import Fixtures

import Bot.Minimax
import Board
import Board.InitialPosition
import Evaluation (discount, mateValue)
import Format.FEN.Import (fromFEN)
import Laws

spec :: Spec
spec = do
  describe "maxValue" $ do
    context "[начальная позиция]" $ do
      it "возращает 0 как максимальное возможное значение для хода для глубины 1" $ do
        fst (maxValue initialBoard 1) `shouldBe` 0

      it "возращает 0 как максимальное возможное значение для хода для глубины 2" $ do
        fst (maxValue initialBoard 2) `shouldBe` 0

    context "[белые]" $ do
      context "[мат в один ход]" $ do
        it "возращает максимально возможное значение как максимальное возможное значение для хода для глубины 1" $ do
          fst (maxValue blackMatedByQueenIn1 1) `shouldBe` (-mateValue)

      context "[мат в два хода]" $ do
        it "находит линейный мат в два хода" $ do
          maxValue whiteLadderMatesIn2 3 `shouldBe` (-mateValue, Move (Piece Rook White) ('e',1) ('g',1))

      context "[мат в 3 хода]" $ do
        it "находит линейный мат в три хода" $ do
          pendingWith "медленный тест"
          maxValue whiteLadderMatesIn3 5 `shouldBe` (-mateValue, Move (Piece Rook White) ('d',2) ('f',2))

      context "[мат один ход (github issue #1)]" $ do
        it "ход 49" $ do
          let board = fromFEN "8/7K/1Q6/8/k7/2p5/PPPP1PPP/1RB3NR w - - 0 49"
          maxValue board 3 `shouldBe` (-mateValue + 2, Move (Piece Pawn White) ('b', 2) ('b', 3))

    context "[чёрные]" $ do
      context "[мат в один ход]" $ do
        it "возращает максимально возможное значение оценки и ход конём" $ do
          maxValue whiteMatedByKninghtIn1 1 `shouldBe` (-mateValue, Move knightBlack ('a', 3) ('c', 2))

      context "[мат в два хода]" $ do
        it "находит линейный мат в два хода" $ do
          maxValue blackLadderMatesIn2 3 `shouldBe` (-mateValue, Move (Piece Rook Black) ('e',1) ('g',1))

        it "белые ставят мат в два хода, но чёрные защищаются, пытаясь взять ладью" $ do
          let board = move whiteLadderMatesIn2ButBlackCanTakeRook $ Move (Piece Rook White) ('e', 1) ('g', 1)
          maxValue board  2 `shouldBe` (-10000, Move (Piece King Black) ('g', 4) ('h', 3))

      context "[мат в три хода]" $ do
        it "находит линейный мат в два хода" $ do
          pendingWith "Slow test"
          maxValue blackLadderMatesIn3 5 `shouldBe` (-mateValue, Move (Piece Rook Black) ('d',2) ('f',2))

      context "защита от мата в 1 ход" $ do
        it "чёрные должны найти возможный мат в 2 хода, белые должны найти защиту от мата в 1 ход" $ do
          pendingWith "найти нормальный пример, в котором противник будет жертвовать качеством, чтобы избежать мата"
