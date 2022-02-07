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
      let (Board squares _) = emptyBoard
      length squares `shouldBe` 0

  describe "takenBy" $ do
    context "[поле не занято ни белыми, ни чёрными]" $ do
      it "возращает False, при запросе занято ли белыми" $ do
        takenBy White emptyBoard ('e', 2) `shouldBe` False

      it "возращает False, при запросе занято ли чёрными" $ do
        takenBy Black emptyBoard ('e', 2) `shouldBe` False

    context "[поле занято белыми]" $ do
      it "возращает True, при запросе занято ли белыми" $ do
        takenBy White initialBoard ('e', 2) `shouldBe` True

      it "возращает False, при запросе занято ли чёрными" $ do
        takenBy Black initialBoard ('e', 2) `shouldBe` False

    context "[поле занято чёрными]" $ do
      it "возращает False, при запросе занято ли белыми" $ do
        takenBy White initialBoard ('e', 7) `shouldBe` False

      it "возращает True, при запросе занято ли чёрными" $ do
        takenBy Black initialBoard ('e', 7) `shouldBe` True

  describe "placePiece" $ do
    it "ставит фигуру на заданную клетку" $ do
      let (Board squares _) = placePiece ('d', 4) pawnWhite emptyBoard 
      M.lookup ('d', 4) squares `shouldBe` Just pawnWhite

  describe "placePieces" $ do
    it "ставит фигуры на заданные клетки" $ do
      let (Board board _) = placePieces [(('a', 2), pawnWhite), (('a', 7), pawnBlack)] emptyBoard 
      M.lookup ('a', 2) board `shouldBe` Just pawnWhite
      M.lookup ('a', 7) board `shouldBe` Just pawnBlack

  describe "move" $ do
    context "ход белых" $ do
      it "белая пешка с начальной позиции" $ do
        let resultBoard = (\(Board b _) -> Map.delete ('e', 2) $ Map.insert ('e', 4) pawnWhite b) initialBoard
        (move initialBoard $ Move pawnWhite ('e',2) ('e',4)) `shouldBe` Board resultBoard (Just ('e', 3))

      it "после хода пешкой через поле устанавливается значение enPassantTarget" $ do
        enPassantTarget (move initialBoard $ Move pawnWhite ('a',2) ('a',4)) `shouldBe` (Just ('a', 3))
