module Laws.RookSpec (spec) where

import Test.Hspec

import           Board
import           Board.InitialPosition
import           Laws.Rook

spec :: Spec
spec = do
  describe "rookMoves" $ do
    it "все ходы ладьи, если ладья находится в центре" $ do
      rookMoves ('d', 4) `shouldBe`
        [
          ('c', 4), ('b', 4), ('a', 4)
        , ('d', 3), ('d', 2), ('d', 1)
        , ('e', 4), ('f', 4), ('g', 4), ('h', 4)
        , ('d', 5), ('d', 6), ('d', 7), ('d', 8)
        ]

  describe "captureThreatSquares" $ do
    it "все угрозы ладьёй фигурам противника" $ do
      captureThreatSquares White ('b', 2) initialBoard `shouldBe` [('b', 7)]

    it "возращает пустой список если нет фигур, которым угрожает ладья" $ do
      captureThreatSquares White ('a', 1) initialBoard `shouldBe` []

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт ладья" $ do
      underAttackSquares initialBoard White ('b', 2) `shouldBe` [('b', 3), ('b', 4), ('b', 5), ('b', 6), ('b', 7)]

    it "возращает пустой список если нет фигур для взятия" $ do
      underAttackSquares (placePiece ('g', 7) pawnWhite initialBoard) White ('a', 1) `shouldBe` []

  describe "captureThreatSquares" $ do
    it "все взятия слона фигур противника" $ do
      captureThreatSquares White ('b', 2) initialBoard `shouldBe` [('b', 7)]

    it "возращает пустой список если нет фигур для взятия" $ do
      captureThreatSquares White ('a', 1) (placePiece ('g', 7) pawnWhite initialBoard) `shouldBe` []

  describe "possibleMoves" $ do
    it "все возможные ходы ладьи на пустой доске" $ do
      let board = placePiece ('d', 4) rookWhite emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move rookWhite ('d', 4) ('c', 4)
        , Move rookWhite ('d', 4) ('b', 4)
        , Move rookWhite ('d', 4) ('a', 4)
        , Move rookWhite ('d', 4) ('d', 3)
        , Move rookWhite ('d', 4) ('d', 2)
        , Move rookWhite ('d', 4) ('d', 1)
        , Move rookWhite ('d', 4) ('e', 4)
        , Move rookWhite ('d', 4) ('f', 4)
        , Move rookWhite ('d', 4) ('g', 4)
        , Move rookWhite ('d', 4) ('h', 4)
        , Move rookWhite ('d', 4) ('d', 5)
        , Move rookWhite ('d', 4) ('d', 6)
        , Move rookWhite ('d', 4) ('d', 7)
        , Move rookWhite ('d', 4) ('d', 8)
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [(('d', 4), rookWhite), (('d', 6), pawnBlack)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ 
          Capture rookWhite ('d', 4) ('d', 6)
        , Move rookWhite ('d', 4) ('c', 4)
        , Move rookWhite ('d', 4) ('b', 4)
        , Move rookWhite ('d', 4) ('a', 4)
        , Move rookWhite ('d', 4) ('d', 3)
        , Move rookWhite ('d', 4) ('d', 2)
        , Move rookWhite ('d', 4) ('d', 1)
        , Move rookWhite ('d', 4) ('e', 4)
        , Move rookWhite ('d', 4) ('f', 4)
        , Move rookWhite ('d', 4) ('g', 4)
        , Move rookWhite ('d', 4) ('h', 4)
        , Move rookWhite ('d', 4) ('d', 5)
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      pending
      let board = placePieces [(('d', 4), rookWhite), (('d', 6), pawnWhite)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move rookWhite ('d', 4) ('c', 4)
        , Move rookWhite ('d', 4) ('b', 4)
        , Move rookWhite ('d', 4) ('a', 4)
        , Move rookWhite ('d', 4) ('d', 3)
        , Move rookWhite ('d', 4) ('d', 2)
        , Move rookWhite ('d', 4) ('d', 1)
        , Move rookWhite ('d', 4) ('e', 4)
        , Move rookWhite ('d', 4) ('f', 4)
        , Move rookWhite ('d', 4) ('g', 4)
        , Move rookWhite ('d', 4) ('h', 4)
        , Move rookWhite ('d', 4) ('d', 5)
        ]   