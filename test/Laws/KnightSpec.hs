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
      knightMoves (read "d4") `shouldBe` map read ["b5", "b3", "c6", "c2", "e6", "e2", "f5", "f3"]

    it "оба хода коня, если конь находится на 'a1'" $ do
      knightMoves (read "a1") `shouldBe` map read ["b3", "c2"]

    it "оба хода коня, если конь находится на 'a1'" $ do
      knightMoves (read "h1") `shouldBe` map read ["f2", "g3"]

    it "оба хода коня, если конь находится на 'h8'" $ do
      knightMoves (read "h8") `shouldBe` map read ["f7", "g6"]

    it "оба хода коня, если конь находится на 'a8'" $ do
      knightMoves (read "a8") `shouldBe` map read ["b6", "c7"]

    it "три хода коня, если конь находится на 'b1'" $ do
      knightMoves (read "b1") `shouldBe` map read ["a3", "c3", "d2"]

  describe "underAttackSquares" $ do
    it "все ходы коня" $ do
      underAttackSquares (read "d4") `shouldBe` map read ["b5", "b3", "c6", "c2", "e6", "e2", "f5", "f3"]

  describe "possibleMoves" $ do
    it "все возможные ходы коня на пустой доске" $ do
      let board = placePiece (read "d4") knightWhite emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move knightWhite (read "d4") (read "b5")
        , Move knightWhite (read "d4") (read "b3")
        , Move knightWhite (read "d4") (read "c6")
        , Move knightWhite (read "d4") (read "c2")
        , Move knightWhite (read "d4") (read "e6")
        , Move knightWhite (read "d4") (read "e2")
        , Move knightWhite (read "d4") (read "f5")
        , Move knightWhite (read "d4") (read "f3")
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [((read "d4"), knightWhite), ((read "e6"), pawnBlack)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Capture knightWhite (read "d4") (read "e6")
        , Move knightWhite (read "d4") (read "b5")
        , Move knightWhite (read "d4") (read "b3")
        , Move knightWhite (read "d4") (read "c6")
        , Move knightWhite (read "d4") (read "c2")
        , Move knightWhite (read "d4") (read "e2")
        , Move knightWhite (read "d4") (read "f5")
        , Move knightWhite (read "d4") (read "f3")
        ]

    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [((read "d4"), knightWhite), ((read "e6"), pawnWhite)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move knightWhite (read "d4") (read "b5")
        , Move knightWhite (read "d4") (read "b3")
        , Move knightWhite (read "d4") (read "c6")
        , Move knightWhite (read "d4") (read "c2")
        , Move knightWhite (read "d4") (read "e2")
        , Move knightWhite (read "d4") (read "f5")
        , Move knightWhite (read "d4") (read "f3")
        ]
