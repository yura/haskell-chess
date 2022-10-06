module Laws.BishopSpec (spec) where

import           Test.Hspec

import           Data.List ( nub )

import           Board
import           Board.InitialPosition
import           Laws.Bishop

spec :: Spec
spec = do
  describe "sw" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      sw (read "d4") `shouldBe` [(read "c3"), (read "b2"), (read "a1")]

    it "возращает пустой список из 'a2'" $
      sw (read "a2") `shouldBe` []

    it "возращает пустой список из 'b1'" $
      sw (read "b1") `shouldBe` []

    it "возращает 'a7' из 'b8'" $
      sw (read "b8") `shouldBe` [(read "a7")]

    it "возращает главную диагональ из 'h8'" $
      sw (read "h8") `shouldBe` [(read "g7"), (read "f6"), (read "e5"), (read "d4"), (read "c3"), (read "b2"), (read "a1")]

    it "возращает пустой список из 'a1..a8' и 'b1..h1'" $ do
      let ss = map Square $ [0, 8..56] ++ [1..7]
      nub (map sw ss) `shouldBe` [[]]

  describe "se" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      se (read "d4") `shouldBe` [(read "e3"), (read "f2"), (read "g1")]

    it "возращает 'b1' из 'a2'" $
      se (read "a2") `shouldBe` [(read "b1")]

    it "возращает пустой список из 'b1'" $
      se (read "b1") `shouldBe` []

    it "возращает всю диагональ из 'b8'" $
      se (read "b8") `shouldBe` [(read "c7"), (read "d6"), (read "e5"), (read "f4"), (read "g3"), (read "h2")]

    it "возращает пустой список из 'h8'" $
      se (read "h8") `shouldBe` []

    it "возращает пустой список из 'a1..h1' и 'h1..h8'" $ do
      let ss = map Square $ [0..7] ++ [15, 23..63]
      nub (map se ss) `shouldBe` [[]]

  describe "ne" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      ne (read "d4") `shouldBe` [(read "e5"), (read "f6"), (read "g7"), (read "h8")]

    it "возращает всю диагональ из 'a2'" $
      ne (read "a2") `shouldBe` [(read "b3"), (read "c4"), (read "d5"), (read "e6"), (read "f7"), (read "g8")]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let ss = map Square $ [56..63] ++ [7,15..63]
      nub (map ne ss) `shouldBe` [[]]

  describe "nw" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      nw (read "d4") `shouldBe` [(read "c5"), (read "b6"), (read "a7")]

    it "возращает всю диагональ из 'h4'" $
      nw (read "h4") `shouldBe` [(read "g5"), (read "f6"), (read "e7"), (read "d8")]

    it "возращает пустой список из 'a8..h8' и 'a1..a8'" $ do
      let ss = map Square $ [56..63] ++ [0,8..56]
      nub (map nw ss) `shouldBe` [[]]

  describe "bishopMoves" $ do
    it "все ходы слона, если слон находится в центре" $ do
      bishopMoves (read "d4") `shouldBe` [ (read "c3"), (read "b2"), (read "a1"), (read "e3"), (read "f2"), (read "g1"), (read "e5"), (read "f6"), (read "g7"), (read "h8"), (read "c5"), (read "b6"), (read "a7") ]

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт слон" $ do
      underAttackSquares initialBoard White (read "b2") `shouldBe` [(read "c3"), (read "d4"), (read "e5"), (read "f6"), (read "g7"), (read "a3")]

  describe "captureThreatSquares" $ do
    it "все взятия слона фигур противника" $ do
      captureThreatSquares White (read "b2") initialBoard `shouldBe` [(read "g7")]

    it "возращает пустой список если нет фигур для взятия" $ do
      captureThreatSquares White (read "a1") (placePiece (read "g7") pawnWhite initialBoard) `shouldBe` []

  describe "possibleMoves" $ do
    it "все возможные ходы слона на пустой доске" $ do
      let board = placePiece (read "d4") bishopWhite emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move bishopWhite (read "d4") (read "c3")
        , Move bishopWhite (read "d4") (read "b2")
        , Move bishopWhite (read "d4") (read "a1")
        , Move bishopWhite (read "d4") (read "e3")
        , Move bishopWhite (read "d4") (read "f2")
        , Move bishopWhite (read "d4") (read "g1")
        , Move bishopWhite (read "d4") (read "e5")
        , Move bishopWhite (read "d4") (read "f6")
        , Move bishopWhite (read "d4") (read "g7")
        , Move bishopWhite (read "d4") (read "h8")
        , Move bishopWhite (read "d4") (read "c5")
        , Move bishopWhite (read "d4") (read "b6")
        , Move bishopWhite (read "d4") (read "a7")
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [((read "d4"), bishopWhite), ((read "g7"), pawnBlack)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ 
          Capture bishopWhite (read "d4") (read "g7")
        , Move bishopWhite (read "d4") (read "c3")
        , Move bishopWhite (read "d4") (read "b2")
        , Move bishopWhite (read "d4") (read "a1")
        , Move bishopWhite (read "d4") (read "e3")
        , Move bishopWhite (read "d4") (read "f2")
        , Move bishopWhite (read "d4") (read "g1")
        , Move bishopWhite (read "d4") (read "e5")
        , Move bishopWhite (read "d4") (read "f6")
        , Move bishopWhite (read "d4") (read "c5")
        , Move bishopWhite (read "d4") (read "b6")
        , Move bishopWhite (read "d4") (read "a7")
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [((read "d4"), bishopWhite), ((read "g7"), pawnWhite)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move bishopWhite (read "d4") (read "c3")
        , Move bishopWhite (read "d4") (read "b2")
        , Move bishopWhite (read "d4") (read "a1")
        , Move bishopWhite (read "d4") (read "e3")
        , Move bishopWhite (read "d4") (read "f2")
        , Move bishopWhite (read "d4") (read "g1")
        , Move bishopWhite (read "d4") (read "e5")
        , Move bishopWhite (read "d4") (read "f6")
        , Move bishopWhite (read "d4") (read "c5")
        , Move bishopWhite (read "d4") (read "b6")
        , Move bishopWhite (read "d4") (read "a7")
        ]        
