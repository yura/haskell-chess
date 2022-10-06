module Laws.RookSpec (spec) where

import Test.Hspec

import           Board
import           Board.InitialPosition
import           Laws.Rook

spec :: Spec
spec = do
  describe "rookMoves" $ do
    it "все ходы ладьи, если ладья находится в центре" $ do
      rookMoves (read "d4") `shouldBe`
        [
          (read "c4"), (read "b4"), (read "a4")
        , (read "d3"), (read "d2"), (read "d1")
        , (read "e4"), (read "f4"), (read "g4"), (read "h4")
        , (read "d5"), (read "d6"), (read "d7"), (read "d8")
        ]

  describe "captureThreatSquares" $ do
    it "все угрозы ладьёй фигурам противника" $ do
      captureThreatSquares White (read "b2") initialBoard `shouldBe` [(read "b7")]

    it "возращает пустой список если нет фигур, которым угрожает ладья" $ do
      captureThreatSquares White (read "a1") initialBoard `shouldBe` []

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт ладья" $ do
      underAttackSquares initialBoard White (read "b2") `shouldBe` [(read "b3"), (read "b4"), (read "b5"), (read "b6"), (read "b7")]

  describe "captureThreatSquares" $ do
    it "все взятия слона фигур противника" $ do
      captureThreatSquares White (read "b2") initialBoard `shouldBe` [(read "b7")]

    it "возращает пустой список если нет фигур для взятия" $ do
      captureThreatSquares White (read "a1") (placePiece (read "g7") pawnWhite initialBoard) `shouldBe` []

  describe "possibleMoves" $ do
    it "все возможные ходы ладьи на пустой доске" $ do
      let board = placePiece (read "d4") rookWhite emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move rookWhite (read "d4") (read "c4")
        , Move rookWhite (read "d4") (read "b4")
        , Move rookWhite (read "d4") (read "a4")
        , Move rookWhite (read "d4") (read "d3")
        , Move rookWhite (read "d4") (read "d2")
        , Move rookWhite (read "d4") (read "d1")
        , Move rookWhite (read "d4") (read "e4")
        , Move rookWhite (read "d4") (read "f4")
        , Move rookWhite (read "d4") (read "g4")
        , Move rookWhite (read "d4") (read "h4")
        , Move rookWhite (read "d4") (read "d5")
        , Move rookWhite (read "d4") (read "d6")
        , Move rookWhite (read "d4") (read "d7")
        , Move rookWhite (read "d4") (read "d8")
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [((read "d4"), rookWhite), ((read "d6"), pawnBlack)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ 
          Capture rookWhite (read "d4") (read "d6")
        , Move rookWhite (read "d4") (read "c4")
        , Move rookWhite (read "d4") (read "b4")
        , Move rookWhite (read "d4") (read "a4")
        , Move rookWhite (read "d4") (read "d3")
        , Move rookWhite (read "d4") (read "d2")
        , Move rookWhite (read "d4") (read "d1")
        , Move rookWhite (read "d4") (read "e4")
        , Move rookWhite (read "d4") (read "f4")
        , Move rookWhite (read "d4") (read "g4")
        , Move rookWhite (read "d4") (read "h4")
        , Move rookWhite (read "d4") (read "d5")
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [((read "d4"), rookWhite), ((read "d6"), pawnWhite)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move rookWhite (read "d4") (read "c4")
        , Move rookWhite (read "d4") (read "b4")
        , Move rookWhite (read "d4") (read "a4")
        , Move rookWhite (read "d4") (read "d3")
        , Move rookWhite (read "d4") (read "d2")
        , Move rookWhite (read "d4") (read "d1")
        , Move rookWhite (read "d4") (read "e4")
        , Move rookWhite (read "d4") (read "f4")
        , Move rookWhite (read "d4") (read "g4")
        , Move rookWhite (read "d4") (read "h4")
        , Move rookWhite (read "d4") (read "d5")
        ]
