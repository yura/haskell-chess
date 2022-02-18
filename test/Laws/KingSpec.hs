module Laws.KingSpec (spec) where

import           Test.Hspec

import qualified Laws
import           Laws.King

import           Board
import           Board.InitialPosition

spec :: Spec
spec = do
  describe "moveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      moveSquares ('h', 6) `shouldBe`
        [
          ('g', 5), ('g', 7)
        , ('g', 6), ('h', 5), ('h', 7)
        ]

  describe "kingValidMoveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      Laws.kingValidMoveSquares White ('h', 4) initialBoard `shouldBe`
        [ ('g', 3), ('g', 5)
        , ('g', 4), ('h', 3), ('h', 5)
        ]

    it "не может приближаться к пешкам" $ do
      let board = movePiece ('e', 1) ('h', 5) kingWhite initialBoard 
      Laws.kingValidMoveSquares White ('h', 5) board `shouldBe`
        [ ('g', 4)
        , ('g', 5), ('h', 4)
        ]

    context "[рокировка]" $ do
      it "не ходит через клетку вперед, если поле занято своей фигурой" pending
      it "не ходит через клетку вперед, если поле занято фигурой противника" pending
      it "не ходит под шах" pending
      it "не встречается с королём (частный случай шаха)" pending
      it "не рокируется, если ходил" pending
      it "не рокируется, если ходила ладья?" pending

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт король" $ do
      let board = placePiece ('c', 3) pawnBlack initialBoard
      underAttackSquares board White ('b', 2) `shouldBe`
        [ ('c', 3)
        , ('a', 3)
        , ('b', 3)]

  describe "possibleMoves" $ do
    it "все возможные ходы королья на пустой доске" $ do
      let board = placePiece ('d', 4) kingWhite emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move kingWhite ('d', 4) ('c', 3)
        , Move kingWhite ('d', 4) ('e', 3)
        , Move kingWhite ('d', 4) ('e', 5)
        , Move kingWhite ('d', 4) ('c', 5)

        , Move kingWhite ('d', 4) ('c', 4)
        , Move kingWhite ('d', 4) ('d', 3)
        , Move kingWhite ('d', 4) ('e', 4)
        , Move kingWhite ('d', 4) ('d', 5)
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [(('d', 4), kingWhite), (('d', 5), pawnBlack), (('e', 5), pawnBlack)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Capture kingWhite ('d', 4) ('e', 5)
        , Capture kingWhite ('d', 4) ('d', 5)
        , Move kingWhite ('d', 4) ('c', 3)
        , Move kingWhite ('d', 4) ('e', 3)
        , Move kingWhite ('d', 4) ('c', 5)
        , Move kingWhite ('d', 4) ('c', 4)
        , Move kingWhite ('d', 4) ('d', 3)
        , Move kingWhite ('d', 4) ('e', 4)
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [(('d', 4), kingWhite), (('d', 5), pawnWhite), (('e', 5), pawnWhite)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move kingWhite ('d', 4) ('c', 3)
        , Move kingWhite ('d', 4) ('e', 3)
        , Move kingWhite ('d', 4) ('c', 5)
        , Move kingWhite ('d', 4) ('c', 4)
        , Move kingWhite ('d', 4) ('d', 3)
        , Move kingWhite ('d', 4) ('e', 4)
        ]