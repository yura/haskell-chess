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

    context "[рокировка]" $ do
      context "[короткая]" $ do
        it "белые рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
          let board = deletePiece ('f', 1) $ deletePiece ('g', 1) initialBoard
          possibleMoves board White ('e', 1) `shouldBe` [KingsideCastling White, Move kingWhite ('e',1) ('f',1)] 

        it "чёрные рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
          let board = deletePiece ('f', 8) $ deletePiece ('g', 8) initialBoard
          possibleMoves board Black ('e', 8) `shouldBe` [KingsideCastling Black, Move kingBlack ('e',8) ('f',8)] 

      it "не рокируется, если между королём и ладьёй стоит фигура" $ do
        possibleMoves initialBoard White ('e', 1) `shouldBe` []

      it "не рокируется, если король не в начальной позиции" $ do
        let board = movePiece ('e', 1) ('d', 1) kingWhite $ deletePiece ('f', 1) $ deletePiece ('g', 1) initialBoard
        possibleMoves board White ('d', 1) `shouldBe` [Move kingWhite ('d',1) ('e',1)] 

      it "не рокируется, если ладья не в начальной позиции" $ do
        let board = movePiece ('h', 1) ('h', 2) rookWhite $ deletePiece ('f', 1) $ deletePiece ('g', 1) initialBoard
        possibleMoves board White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('f',1)]

      it "не рокируется, если ходил" pending
      it "не рокируется, если ходила ладья" pending
      it "не рокируется, если король под шахом" pending
      it "не рокируется, если король пройдёт через битое поле" pending
      it "не рокируется под шах" pending
      it "ладья может рокироваться через битое поле" pending
