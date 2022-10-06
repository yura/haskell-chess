module Format.FEN.ImportSpec (spec) where

import Test.Hspec

import qualified Data.Map as M

import Board
import Board.InitialPosition
import Format.FEN.Import
import qualified Format.FEN as E
import Text.Parsec (parse)

spec :: Spec
spec = do
  describe "fromFEN & toFEN" $ do
    it "значение не меняется для начальное позиции" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
      E.toFEN (fromFEN fen) `shouldBe` fen

    it "значение не меняется после хода 1. e4" $ do
      let fen = "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
      E.toFEN (fromFEN fen) `shouldBe` fen

    it "значение не меняется после хода 1... c5" $ do
      let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/8/PPPP1PPP/RNBQKBNR w KQkq c6 0 2"
      E.toFEN (fromFEN fen) `shouldBe` fen

    it "значение не меняется после хода 2. Nf3" $ do
      let fen = "rnbqkbnr/pp1ppppp/8/2p5/4P3/5N2/PPPP1PPP/RNBQKB1R b KQkq - 1 2"
      E.toFEN (fromFEN fen) `shouldBe` fen

  describe "parseSquares" $ do
    it "возращает пустую фигуру" $ do
      parseSquares "8/8/8/8/8/8/8/8" `shouldBe` squares emptyBoard

    it "возращает фигуры для начальной позиции" $ do
      parseSquares "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR" `shouldBe` squares initialBoard

  describe "parseRow" $ do
    it "возращает пустую фигуру" $ do
      parseRow "8" `shouldBe` replicate 8 Nothing

    it "возращает фигуры для начальной позиции" $ do
      parseRow "prnbqk" `shouldBe`
          [ Just pawnBlack
          , Just rookBlack
          , Just knightBlack
          , Just bishopBlack
          , Just queenBlack
          , Just kingBlack
          ]

    it "возращает фигуры для начальной позиции" $ do
      parseRow "4P3" `shouldBe` [ Nothing, Nothing, Nothing, Nothing, Just pawnWhite, Nothing, Nothing, Nothing ]

    it "отрабатывает без ошибки RNBQKB1R" $ do
      --parseRow "Q" 'a' 4 `shouldBe`
      parseRow "RNBQKB1R" `shouldBe`

        [ Just rookWhite
        , Just knightWhite
        , Just bishopWhite
        , Just queenWhite
        , Just kingWhite
        , Just bishopWhite
        , Nothing
        , Just rookWhite
        ]
