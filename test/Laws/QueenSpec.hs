module Laws.QueenSpec (spec) where

import           Test.Hspec

import           Board
import           Board.InitialPosition
import           Laws.Queen

spec :: Spec
spec = do
  describe "queenMoves" $ do
    it "все ходы ферзя, если ферзь находится в центре" $ do
      queenMoves ('e', 7) `shouldBe`
        [
          ('d', 6), ('c', 5), ('b', 4), ('a', 3)
        , ('f', 6), ('g', 5), ('h', 4)
        , ('f', 8)
        , ('d', 8)
        , ('d', 7), ('c', 7), ('b', 7), ('a', 7)
        , ('e', 6), ('e', 5), ('e', 4), ('e', 3), ('e', 2), ('e', 1)
        , ('f', 7), ('g', 7), ('h', 7)
        , ('e', 8)
        ]

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт ферзь" $ do
      underAttackSquares initialBoard White ('b', 2) `shouldBe`
        [ ('c', 3)
        , ('d', 4)
        , ('e', 5)
        , ('f', 6)
        , ('g', 7)
        , ('a', 3)
        , ('b', 3)
        , ('b', 4)
        , ('b', 5)
        , ('b', 6)
        , ('b', 7)]

  describe "captureThreatSquares" $ do
    it "все угрозы ладьёй фигурам противника" $ do
      captureThreatSquares White ('b', 2) initialBoard `shouldBe` [('g', 7), ('b', 7)]

    it "возращает пустой список если нет фигур, которым угрожает ладья" $ do
      captureThreatSquares White ('a', 1) initialBoard `shouldBe` []

  describe "possibleMoves" $ do
    it "все возможные ходы ферзя на пустой доске" $ do
      let board = placePiece ('d', 4) queenWhite emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move queenWhite ('d', 4) ('c', 3)
        , Move queenWhite ('d', 4) ('b', 2)
        , Move queenWhite ('d', 4) ('a', 1)
        , Move queenWhite ('d', 4) ('e', 3)
        , Move queenWhite ('d', 4) ('f', 2)
        , Move queenWhite ('d', 4) ('g', 1)
        , Move queenWhite ('d', 4) ('e', 5)
        , Move queenWhite ('d', 4) ('f', 6)
        , Move queenWhite ('d', 4) ('g', 7)
        , Move queenWhite ('d', 4) ('h', 8)
        , Move queenWhite ('d', 4) ('c', 5)
        , Move queenWhite ('d', 4) ('b', 6)
        , Move queenWhite ('d', 4) ('a', 7)

        , Move queenWhite ('d', 4) ('c', 4)
        , Move queenWhite ('d', 4) ('b', 4)
        , Move queenWhite ('d', 4) ('a', 4)
        , Move queenWhite ('d', 4) ('d', 3)
        , Move queenWhite ('d', 4) ('d', 2)
        , Move queenWhite ('d', 4) ('d', 1)
        , Move queenWhite ('d', 4) ('e', 4)
        , Move queenWhite ('d', 4) ('f', 4)
        , Move queenWhite ('d', 4) ('g', 4)
        , Move queenWhite ('d', 4) ('h', 4)
        , Move queenWhite ('d', 4) ('d', 5)
        , Move queenWhite ('d', 4) ('d', 6)
        , Move queenWhite ('d', 4) ('d', 7)
        , Move queenWhite ('d', 4) ('d', 8)
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [(('d', 4), queenWhite), (('d', 6), pawnBlack), (('g', 7), pawnBlack)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Capture queenWhite ('d', 4) ('g', 7)
        , Capture queenWhite ('d', 4) ('d', 6)
        , Move queenWhite ('d', 4) ('c', 3)
        , Move queenWhite ('d', 4) ('b', 2)
        , Move queenWhite ('d', 4) ('a', 1)
        , Move queenWhite ('d', 4) ('e', 3)
        , Move queenWhite ('d', 4) ('f', 2)
        , Move queenWhite ('d', 4) ('g', 1)
        , Move queenWhite ('d', 4) ('e', 5)
        , Move queenWhite ('d', 4) ('f', 6)
        , Move queenWhite ('d', 4) ('c', 5)
        , Move queenWhite ('d', 4) ('b', 6)
        , Move queenWhite ('d', 4) ('a', 7)
        , Move queenWhite ('d', 4) ('c', 4)
        , Move queenWhite ('d', 4) ('b', 4)
        , Move queenWhite ('d', 4) ('a', 4)
        , Move queenWhite ('d', 4) ('d', 3)
        , Move queenWhite ('d', 4) ('d', 2)
        , Move queenWhite ('d', 4) ('d', 1)
        , Move queenWhite ('d', 4) ('e', 4)
        , Move queenWhite ('d', 4) ('f', 4)
        , Move queenWhite ('d', 4) ('g', 4)
        , Move queenWhite ('d', 4) ('h', 4)
        , Move queenWhite ('d', 4) ('d', 5)
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [(('d', 4), queenWhite), (('d', 6), pawnWhite), (('g', 7), bishopWhite)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move queenWhite ('d', 4) ('c', 3)
        , Move queenWhite ('d', 4) ('b', 2)
        , Move queenWhite ('d', 4) ('a', 1)
        , Move queenWhite ('d', 4) ('e', 3)
        , Move queenWhite ('d', 4) ('f', 2)
        , Move queenWhite ('d', 4) ('g', 1)
        , Move queenWhite ('d', 4) ('e', 5)
        , Move queenWhite ('d', 4) ('f', 6)
        , Move queenWhite ('d', 4) ('c', 5)
        , Move queenWhite ('d', 4) ('b', 6)
        , Move queenWhite ('d', 4) ('a', 7)
        , Move queenWhite ('d', 4) ('c', 4)
        , Move queenWhite ('d', 4) ('b', 4)
        , Move queenWhite ('d', 4) ('a', 4)
        , Move queenWhite ('d', 4) ('d', 3)
        , Move queenWhite ('d', 4) ('d', 2)
        , Move queenWhite ('d', 4) ('d', 1)
        , Move queenWhite ('d', 4) ('e', 4)
        , Move queenWhite ('d', 4) ('f', 4)
        , Move queenWhite ('d', 4) ('g', 4)
        , Move queenWhite ('d', 4) ('h', 4)
        , Move queenWhite ('d', 4) ('d', 5)
        ]

    it "не может ходить, открывая короля https://lichess.org/editor/2k5/8/8/8/8/8/2KQ3r/8_w_-_-_0_1" $ do
      pending
      let board = placePieces [(('c', 2), kingWhite), (('d', 2), queenWhite), (('g', 2), rookBlack)] emptyBoard
      possibleMoves board White ('d', 2) `shouldBe`
        [ Capture queenWhite ('d', 2) ('g', 2)
        , Move queenWhite ('d', 2) ('e', 2)
        , Move queenWhite ('d', 2) ('f', 2)
        ]
