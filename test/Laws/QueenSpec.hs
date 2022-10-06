module Laws.QueenSpec (spec) where

import           Test.Hspec

import           Board
import           Board.InitialPosition
import           Laws.Queen

spec :: Spec
spec = do
  describe "queenMoves" $ do
    it "все ходы ферзя, если ферзь находится в центре" $ do
      queenMoves (read "e7") `shouldBe`
        [
          (read "d6"), (read "c5"), (read "b4"), (read "a3")
        , (read "f6"), (read "g5"), (read "h4")
        , (read "f8")
        , (read "d8")
        , (read "d7"), (read "c7"), (read "b7"), (read "a7")
        , (read "e6"), (read "e5"), (read "e4"), (read "e3"), (read "e2"), (read "e1")
        , (read "f7"), (read "g7"), (read "h7")
        , (read "e8")
        ]

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт ферзь" $ do
      underAttackSquares initialBoard White (read "b2") `shouldBe`
        [ (read "c3")
        , (read "d4")
        , (read "e5")
        , (read "f6")
        , (read "g7")
        , (read "a3")
        , (read "b3")
        , (read "b4")
        , (read "b5")
        , (read "b6")
        , (read "b7")]

  describe "captureThreatSquares" $ do
    it "все угрозы ладьёй фигурам противника" $ do
      captureThreatSquares White (read "b2") initialBoard `shouldBe` [(read "g7"), (read "b7")]

    it "возращает пустой список если нет фигур, которым угрожает ладья" $ do
      captureThreatSquares White (read "a1") initialBoard `shouldBe` []

  describe "possibleMoves" $ do
    it "все возможные ходы ферзя на пустой доске" $ do
      let board = placePiece (read "d4") queenWhite emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move queenWhite (read "d4") (read "c3")
        , Move queenWhite (read "d4") (read "b2")
        , Move queenWhite (read "d4") (read "a1")
        , Move queenWhite (read "d4") (read "e3")
        , Move queenWhite (read "d4") (read "f2")
        , Move queenWhite (read "d4") (read "g1")
        , Move queenWhite (read "d4") (read "e5")
        , Move queenWhite (read "d4") (read "f6")
        , Move queenWhite (read "d4") (read "g7")
        , Move queenWhite (read "d4") (read "h8")
        , Move queenWhite (read "d4") (read "c5")
        , Move queenWhite (read "d4") (read "b6")
        , Move queenWhite (read "d4") (read "a7")

        , Move queenWhite (read "d4") (read "c4")
        , Move queenWhite (read "d4") (read "b4")
        , Move queenWhite (read "d4") (read "a4")
        , Move queenWhite (read "d4") (read "d3")
        , Move queenWhite (read "d4") (read "d2")
        , Move queenWhite (read "d4") (read "d1")
        , Move queenWhite (read "d4") (read "e4")
        , Move queenWhite (read "d4") (read "f4")
        , Move queenWhite (read "d4") (read "g4")
        , Move queenWhite (read "d4") (read "h4")
        , Move queenWhite (read "d4") (read "d5")
        , Move queenWhite (read "d4") (read "d6")
        , Move queenWhite (read "d4") (read "d7")
        , Move queenWhite (read "d4") (read "d8")
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [((read "d4"), queenWhite), ((read "d6"), pawnBlack), ((read "g7"), pawnBlack)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Capture queenWhite (read "d4") (read "g7")
        , Capture queenWhite (read "d4") (read "d6")
        , Move queenWhite (read "d4") (read "c3")
        , Move queenWhite (read "d4") (read "b2")
        , Move queenWhite (read "d4") (read "a1")
        , Move queenWhite (read "d4") (read "e3")
        , Move queenWhite (read "d4") (read "f2")
        , Move queenWhite (read "d4") (read "g1")
        , Move queenWhite (read "d4") (read "e5")
        , Move queenWhite (read "d4") (read "f6")
        , Move queenWhite (read "d4") (read "c5")
        , Move queenWhite (read "d4") (read "b6")
        , Move queenWhite (read "d4") (read "a7")
        , Move queenWhite (read "d4") (read "c4")
        , Move queenWhite (read "d4") (read "b4")
        , Move queenWhite (read "d4") (read "a4")
        , Move queenWhite (read "d4") (read "d3")
        , Move queenWhite (read "d4") (read "d2")
        , Move queenWhite (read "d4") (read "d1")
        , Move queenWhite (read "d4") (read "e4")
        , Move queenWhite (read "d4") (read "f4")
        , Move queenWhite (read "d4") (read "g4")
        , Move queenWhite (read "d4") (read "h4")
        , Move queenWhite (read "d4") (read "d5")
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [((read "d4"), queenWhite), ((read "d6"), pawnWhite), ((read "g7"), bishopWhite)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move queenWhite (read "d4") (read "c3")
        , Move queenWhite (read "d4") (read "b2")
        , Move queenWhite (read "d4") (read "a1")
        , Move queenWhite (read "d4") (read "e3")
        , Move queenWhite (read "d4") (read "f2")
        , Move queenWhite (read "d4") (read "g1")
        , Move queenWhite (read "d4") (read "e5")
        , Move queenWhite (read "d4") (read "f6")
        , Move queenWhite (read "d4") (read "c5")
        , Move queenWhite (read "d4") (read "b6")
        , Move queenWhite (read "d4") (read "a7")
        , Move queenWhite (read "d4") (read "c4")
        , Move queenWhite (read "d4") (read "b4")
        , Move queenWhite (read "d4") (read "a4")
        , Move queenWhite (read "d4") (read "d3")
        , Move queenWhite (read "d4") (read "d2")
        , Move queenWhite (read "d4") (read "d1")
        , Move queenWhite (read "d4") (read "e4")
        , Move queenWhite (read "d4") (read "f4")
        , Move queenWhite (read "d4") (read "g4")
        , Move queenWhite (read "d4") (read "h4")
        , Move queenWhite (read "d4") (read "d5")
        ]

    it "не может ходить, открывая короля https://lichess.org/editor/2k5/8/8/8/8/8/2KQ3r/8_w_-_-_0_1" $ do
      pending
      let board = placePieces [((read "c2"), kingWhite), ((read "d2"), queenWhite), ((read "g2"), rookBlack)] emptyBoard
      possibleMoves board White (read "d2") `shouldBe`
        [ Capture queenWhite (read "d2") (read "g2")
        , Move queenWhite (read "d2") (read "e2")
        , Move queenWhite (read "d2") (read "f2")
        ]
