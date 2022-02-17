module Laws.KnightSpec (spec) where

import           Test.Hspec
import           Board
import           Laws.Knight
--import Laws (possibleMoves)
import Control.Applicative (Alternative(empty))

spec :: Spec
spec = do
  describe "knightMoves" $ do
    it "все ходы коня, если конь находится в центре" $ do
      knightMoves ('d', 4) `shouldBe` [('b', 3), ('c', 2), ('e', 2), ('f', 3), ('f', 5), ('e', 6), ('c', 6), ('b', 5)]

    it "оба хода коня, если конь находится на 'a1'" $ do
      knightMoves ('a', 1) `shouldBe` [('c', 2), ('b', 3)]

    it "оба хода коня, если конь находится на 'a1'" $ do
      knightMoves ('h', 1) `shouldBe` [('g', 3), ('f', 2)]

    it "оба хода коня, если конь находится на 'h8'" $ do
      knightMoves ('h', 8) `shouldBe` [('f', 7), ('g', 6)]

    it "оба хода коня, если конь находится на 'a8'" $ do
      knightMoves ('a', 8) `shouldBe` [('b', 6), ('c', 7)]

    it "три хода коня, если конь находится на 'b1'" $ do
      knightMoves ('b', 1) `shouldBe` [('d', 2), ('c', 3), ('a', 3)]

  describe "underAttackSquares" $ do
    it "все ходы коня" $ do
      underAttackSquares ('d', 4) `shouldBe` [('b', 3), ('c', 2), ('e', 2), ('f', 3), ('f', 5), ('e', 6), ('c', 6), ('b', 5)]

  describe "possibleMoves" $ do
    it "все возможные ходы коня на пустой доске" $ do
      let board = placePiece ('d', 4) knightWhite emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move knightWhite ('d', 4) ('b', 3)
        , Move knightWhite ('d', 4) ('c', 2)
        , Move knightWhite ('d', 4) ('e', 2)
        , Move knightWhite ('d', 4) ('f', 3)
        , Move knightWhite ('d', 4) ('f', 5)
        , Move knightWhite ('d', 4) ('e', 6)
        , Move knightWhite ('d', 4) ('c', 6)
        , Move knightWhite ('d', 4) ('b', 5)
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [(('d', 4), knightWhite), (('e', 6), pawnBlack)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Capture knightWhite ('d', 4) ('e', 6)
        , Move knightWhite ('d', 4) ('b', 3)
        , Move knightWhite ('d', 4) ('c', 2)
        , Move knightWhite ('d', 4) ('e', 2)
        , Move knightWhite ('d', 4) ('f', 3)
        , Move knightWhite ('d', 4) ('f', 5)
        , Move knightWhite ('d', 4) ('c', 6)
        , Move knightWhite ('d', 4) ('b', 5)
        ]

    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [(('d', 4), knightWhite), (('e', 6), pawnWhite)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move knightWhite ('d', 4) ('b', 3)
        , Move knightWhite ('d', 4) ('c', 2)
        , Move knightWhite ('d', 4) ('e', 2)
        , Move knightWhite ('d', 4) ('f', 3)
        , Move knightWhite ('d', 4) ('f', 5)
        , Move knightWhite ('d', 4) ('c', 6)
        , Move knightWhite ('d', 4) ('b', 5)
        ]