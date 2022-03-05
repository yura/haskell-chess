module Format.FEN.ImportSpec (spec) where

import Test.Hspec

import qualified Data.Map as M

import Board
import Board.InitialPosition
import Format.FEN.Import

spec :: Spec
spec = do
  describe "parseSquares" $ do
    it "возращает пустую фигуру" $ do
      parseSquares "8/8/8/8/8/8/8/8" `shouldBe` squares emptyBoard

    it "возращает фигуры для начальной позиции" $ do
      parseSquares "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" `shouldBe` squares initialBoard

  describe "parseRow" $ do
    it "возращает пустую фигуру" $ do
      parseRow "8" 'b' 7 `shouldBe` []

    it "возращает фигуры для начальной позиции" $ do
      parseRow "prnbqk" 'a' 5 `shouldBe`
          [ (('a', 5), pawnBlack)
          , (('b', 5), rookBlack)
          , (('c', 5), knightBlack)
          , (('d', 5), bishopBlack)
          , (('e', 5), queenBlack)
          , (('f', 5), kingBlack)
          ]

    it "возращает фигуры для начальной позиции" $ do
      parseRow "4P3" 'a' 4 `shouldBe` [ (('e', 4), pawnWhite) ]