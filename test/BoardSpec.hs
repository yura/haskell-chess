{-# LANGUAGE OverloadedStrings #-}

module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, pending )

import qualified Data.Map as M
import Data.List ( nub )
import Board

spec :: Spec
spec = do
  describe "squareNames" $ do
    it "генерирует 64 названия клеток" $
      length squareNames `shouldBe` 64
  

  describe "emptyBoard" $ do
    it "генерирует пустую доску размером 64 клетки" $ do
      let (Board squares) = emptyBoard
      length squares `shouldBe` 64


  describe "placePiece" $ do
    it "уставнавливает фигуру на заданную клетку" $ do
      let whitePawn = Piece Pawn White
      let (Board squares) = placePiece ('d', 4) whitePawn emptyBoard
      M.lookup ('d', 4) squares `shouldBe` Just (Square ('d', 4) $ Just whitePawn)


  describe "bottomLeft" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      bottomLeft ('d', 4) `shouldBe` [('c', 3), ('b', 2), ('a', 1)]

    it "возращает пустой список из 'a2'" $
      bottomLeft ('a', 2) `shouldBe` []

    it "возращает пустой список из 'b1'" $
      bottomLeft ('b', 1) `shouldBe` []

    it "возращает 'a7' из 'b8'" $
      bottomLeft ('b', 8) `shouldBe` [('a', 7)]

    it "возращает главную диагональ из 'h8'" $
      bottomLeft ('h', 8) `shouldBe` [('g', 7), ('f', 6), ('e', 5), ('d', 4), ('c', 3), ('b', 2), ('a', 1)]

    it "возращает пустой список из 'a1..a8' и 'a1..h1'" $ do
      let names = [(r, 1) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map (\n -> bottomLeft n) names
      nub result `shouldBe` [[]]
      
  describe "bottomRight" $ do
    it "возращает все имена из центра в начало по главной диагонали" $
      bottomRight ('d', 4) `shouldBe` [('e', 3), ('f', 2), ('g', 1)]

    it "возращает 'b1' из 'a2'" $
      bottomRight ('a', 2) `shouldBe` [('b', 1)]

    it "возращает пустой список из 'b1'" $
      bottomRight ('b', 1) `shouldBe` []

    it "возращает всю диагональ из 'b8'" $
      bottomRight ('b', 8) `shouldBe` [('c', 7), ('d', 6), ('e', 5), ('f', 4), ('g', 3), ('h', 2)]

    it "возращает пустой список из 'h8'" $
      bottomRight ('h', 8) `shouldBe` []

    it "возращает пустой список из 'a1..h1' и 'h1..h8'" $ do
      let names = [(r, 1) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map (\n -> bottomRight n) names
      nub result `shouldBe` [[]]

  describe "topRight" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      topRight ('d', 4) `shouldBe` [('e', 5), ('f', 6), ('g', 7), ('h', 8)]

    it "возращает всю диагональ из 'a2'" $
      topRight ('a', 2) `shouldBe` [('b', 3), ('c', 4), ('d', 5), ('e', 6), ('f', 7), ('g', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('h', c) | c <- [1..8]]
      let result = map (\n -> topRight n) names
      nub result `shouldBe` [[]]

  describe "topLeft" $ do
    it "возращает все имена из центра в конец по главной диагонали" $
      topLeft ('d', 4) `shouldBe` [('c', 5), ('b', 6), ('a', 7)]

    it "возращает всю диагональ из 'h4'" $
      topLeft ('h', 4) `shouldBe` [('g', 5), ('f', 6), ('e', 7), ('d', 8)]

    it "возращает пустой список из 'a8..h8' и 'h1..h8'" $ do
      let names = [(r, 8) | r <- ['a'..'h']] ++ [('a', c) | c <- [1..8]]
      let result = map (\n -> topLeft n) names
      nub result `shouldBe` [[]]

  describe "bishopPossibleMoves" $ do
    it "все возможные ходы слона, если слон находится в центре" $ do
      pending
      --bishopPossibleMoves emptyBoard "d4" `shouldBe` [ "a1", "b2", "c3", "e5", "f6", "g7", "h8", "g1", "f2", "e3", "c5" "b6" "a7" ]  

