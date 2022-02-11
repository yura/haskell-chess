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
      let result = map (\n -> sw n) names
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
      let result = map (\n -> se n) names
      nub result `shouldBe` [[]]

  describe "ne" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      ne ('d', 4) `shouldBe` [('e', 5), ('f', 6), ('g', 7), ('h', 8)]

    it "возращает всю диагональ из 'a2'" $
      ne ('a', 2) `shouldBe` [('b', 3), ('c', 4), ('d', 5), ('e', 6), ('f', 7), ('g', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map (\n -> ne n) names
      nub result `shouldBe` [[]]

  describe "nw" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      nw ('d', 4) `shouldBe` [('c', 5), ('b', 6), ('a', 7)]

    it "возращает всю диагональ из 'h4'" $
      nw ('h', 4) `shouldBe` [('g', 5), ('f', 6), ('e', 7), ('d', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map (\n -> nw n) names
      nub result `shouldBe` [[]]

  describe "bishopMoves" $ do
    it "все ходы слона, если слон находится в центре" $ do
      bishopMoves ('d', 4) `shouldBe` [ ('c', 3), ('b', 2), ('a', 1), ('e', 3), ('f', 2), ('g', 1), ('e', 5), ('f', 6), ('g', 7), ('h', 8), ('c', 5), ('b', 6), ('a', 7) ]

  describe "bishopCaptures" $ do
    it "все взятия слона фигур противника" $ do
      bishopCaptures White ('b', 2) initialBoard `shouldBe` [('g', 7)]

    it "возращает пустой список если нет фигур для взятия" $ do
      bishopCaptures White ('a', 1) (placePiece ('g', 7) pawnWhite initialBoard) `shouldBe` []

