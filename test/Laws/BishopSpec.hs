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
      sw ('d', 4) `shouldBe` [('c', 3), ('b', 2), ('a', 1)]

    it "возращает пустой список из 'a2'" $
      sw ('a', 2) `shouldBe` []

    it "возращает пустой список из 'b1'" $
      sw ('b', 1) `shouldBe` []

    it "возращает 'a7' из 'b8'" $
      sw ('b', 8) `shouldBe` [('a', 7)]

    it "возращает главную диагональ из 'h8'" $
      sw ('h', 8) `shouldBe` [('g', 7), ('f', 6), ('e', 5), ('d', 4), ('c', 3), ('b', 2), ('a', 1)]

    it "возращает пустой список из 'a1..a8' и 'a1..h1'" $ do
      let names = [(r, 1) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map sw names
      nub result `shouldBe` [[]]

  describe "se" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      se ('d', 4) `shouldBe` [('e', 3), ('f', 2), ('g', 1)]

    it "возращает 'b1' из 'a2'" $
      se ('a', 2) `shouldBe` [('b', 1)]

    it "возращает пустой список из 'b1'" $
      se ('b', 1) `shouldBe` []

    it "возращает всю диагональ из 'b8'" $
      se ('b', 8) `shouldBe` [('c', 7), ('d', 6), ('e', 5), ('f', 4), ('g', 3), ('h', 2)]

    it "возращает пустой список из 'h8'" $
      se ('h', 8) `shouldBe` []

    it "возращает пустой список из 'a1..h1' и 'h1..h8'" $ do
      let names = [(r, 1) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map se names
      nub result `shouldBe` [[]]

  describe "ne" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      ne ('d', 4) `shouldBe` [('e', 5), ('f', 6), ('g', 7), ('h', 8)]

    it "возращает всю диагональ из 'a2'" $
      ne ('a', 2) `shouldBe` [('b', 3), ('c', 4), ('d', 5), ('e', 6), ('f', 7), ('g', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map ne names
      nub result `shouldBe` [[]]

  describe "nw" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      nw ('d', 4) `shouldBe` [('c', 5), ('b', 6), ('a', 7)]

    it "возращает всю диагональ из 'h4'" $
      nw ('h', 4) `shouldBe` [('g', 5), ('f', 6), ('e', 7), ('d', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map nw names
      nub result `shouldBe` [[]]

  describe "bishopMoves" $ do
    it "все ходы слона, если слон находится в центре" $ do
      bishopMoves ('d', 4) `shouldBe` [ ('c', 3), ('b', 2), ('a', 1), ('e', 3), ('f', 2), ('g', 1), ('e', 5), ('f', 6), ('g', 7), ('h', 8), ('c', 5), ('b', 6), ('a', 7) ]

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт слон" $ do
      underAttackSquares initialBoard White ('b', 2) `shouldBe` [('c', 3), ('d', 4), ('e', 5), ('f', 6), ('g', 7), ('a', 3)]

  describe "captureThreatSquares" $ do
    it "все взятия слона фигур противника" $ do
      captureThreatSquares White ('b', 2) initialBoard `shouldBe` [('g', 7)]

    it "возращает пустой список если нет фигур для взятия" $ do
      captureThreatSquares White ('a', 1) (placePiece ('g', 7) pawnWhite initialBoard) `shouldBe` []

  describe "possibleMoves" $ do
    it "все возможные ходы слона на пустой доске" $ do
      let board = placePiece ('d', 4) bishopWhite emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move bishopWhite ('d', 4) ('c', 3)
        , Move bishopWhite ('d', 4) ('b', 2)
        , Move bishopWhite ('d', 4) ('a', 1)
        , Move bishopWhite ('d', 4) ('e', 3)
        , Move bishopWhite ('d', 4) ('f', 2)
        , Move bishopWhite ('d', 4) ('g', 1)
        , Move bishopWhite ('d', 4) ('e', 5)
        , Move bishopWhite ('d', 4) ('f', 6)
        , Move bishopWhite ('d', 4) ('g', 7)
        , Move bishopWhite ('d', 4) ('h', 8)
        , Move bishopWhite ('d', 4) ('c', 5)
        , Move bishopWhite ('d', 4) ('b', 6)
        , Move bishopWhite ('d', 4) ('a', 7)
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [(('d', 4), bishopWhite), (('g', 7), pawnBlack)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ 
          Capture bishopWhite ('d', 4) ('g', 7)
        , Move bishopWhite ('d', 4) ('c', 3)
        , Move bishopWhite ('d', 4) ('b', 2)
        , Move bishopWhite ('d', 4) ('a', 1)
        , Move bishopWhite ('d', 4) ('e', 3)
        , Move bishopWhite ('d', 4) ('f', 2)
        , Move bishopWhite ('d', 4) ('g', 1)
        , Move bishopWhite ('d', 4) ('e', 5)
        , Move bishopWhite ('d', 4) ('f', 6)
        , Move bishopWhite ('d', 4) ('c', 5)
        , Move bishopWhite ('d', 4) ('b', 6)
        , Move bishopWhite ('d', 4) ('a', 7)
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [(('d', 4), bishopWhite), (('g', 7), pawnWhite)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move bishopWhite ('d', 4) ('c', 3)
        , Move bishopWhite ('d', 4) ('b', 2)
        , Move bishopWhite ('d', 4) ('a', 1)
        , Move bishopWhite ('d', 4) ('e', 3)
        , Move bishopWhite ('d', 4) ('f', 2)
        , Move bishopWhite ('d', 4) ('g', 1)
        , Move bishopWhite ('d', 4) ('e', 5)
        , Move bishopWhite ('d', 4) ('f', 6)
        , Move bishopWhite ('d', 4) ('c', 5)
        , Move bishopWhite ('d', 4) ('b', 6)
        , Move bishopWhite ('d', 4) ('a', 7)
        ]        
