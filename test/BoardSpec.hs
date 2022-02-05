module BoardSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map
import           Board
import           Board.InitialPosition

spec :: Spec
spec = do
  describe "squareNames" $ do
    it "генерирует 64 названия клеток" $
      length squareNames `shouldBe` 64

  describe "emptyBoard" $ do
    it "генерирует пустую доску размером 0 клеток" $ do
      let (Board squares) = emptyBoard
      length squares `shouldBe` 0

  describe "placePiece" $ do
    it "ставит фигуру на заданную клетку" $ do
      let whitePawn = Piece Pawn White
      let (Board squares) = placePiece ('d', 4) whitePawn emptyBoard 
      M.lookup ('d', 4) squares `shouldBe` Just whitePawn

  describe "placePieces" $ do
    it "ставит фигуры на заданные клетки" $ do
      let whitePawn = Piece Pawn White
      let blackPawn = Piece Pawn Black
      let (Board board) = placePieces [(('a', 2), whitePawn), (('a', 7), blackPawn)] emptyBoard 
      M.lookup ('a', 2) board `shouldBe` Just whitePawn
      M.lookup ('a', 7) board `shouldBe` Just blackPawn

  describe "move" $ do
    context "ход белых" $ do
      it "белая пешка с начальной позиции" $ do
        let resultBoard = (\(Board b) -> Map.delete ('e', 2) $ Map.insert ('e', 4) (Piece Pawn White) b) initialBoard
        (move initialBoard $ Move (Piece Pawn White) ('e',2) ('e',4)) `shouldBe` Board resultBoard

