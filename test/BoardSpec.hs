{-# LANGUAGE OverloadedStrings #-}

module BoardSpec (spec) where

import Test.Hspec ( describe, it, shouldBe, Spec, pending )

import qualified Data.Map as M
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
      let (Board squares) = placePiece "d4" whitePawn emptyBoard
      M.lookup "d4" squares `shouldBe` Just (Square "d4" $ Just whitePawn)

  describe "bishopPossibleMoves" $ do
    it "" $
      pending

