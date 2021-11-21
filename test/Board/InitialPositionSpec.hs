module Board.InitialPositionSpec (spec) where

import Test.Hspec

import Data.List ( nub )

import Board
import Board.InitialPosition

spec :: Spec
spec = do
  describe "initialBoard" $ do
    it "возвращает 32 клетки" $ do
      let (Board squares) = initialBoard
      length squares `shouldBe` 32

  describe "pieces" $ do
    it "возвращает 32 фигуры" $
      length pieces `shouldBe` 32

  describe "whitePieces" $ do
    it "возвращает 16 фигур" $
      length whitePieces `shouldBe` 16

    it "все фигуры белые" $
      (head . nub) (map (\(_, Piece _ color) -> color) whitePieces) `shouldBe` White

  describe "blackPieces" $ do
    it "возвращает 16 фигур" $
      length blackPieces `shouldBe` 16

    it "все фигуры белые" $
      (head . nub) (map (\(_, Piece _ color) -> color) blackPieces) `shouldBe` Black

  describe "pieceRow" $ do
    it "белые пешки стоят на втором ряду" $
      pieceRow Pawn White `shouldBe` 2

    it "черные пешки стоят на седьмом ряду" $
      pieceRow Pawn Black `shouldBe` 7

    it "белые ладьи стоят на первом ряду" $
      pieceRow Rook White `shouldBe` 1

    it "черные ладьи стоят на восьмом ряду" $
      pieceRow Rook Black `shouldBe` 8

    it "белые кони стоят на первом ряду" $
      pieceRow Knight White `shouldBe` 1

    it "черные кони стоят на восьмом ряду" $
      pieceRow Knight Black `shouldBe` 8

    it "белые слоны стоят на первом ряду" $
      pieceRow Bishop White `shouldBe` 1

    it "черные слоны стоят на восьмом ряду" $
      pieceRow Bishop Black `shouldBe` 8

    it "белый ферзь стоит на первом ряду" $
      pieceRow Queen White `shouldBe` 1

    it "черный ферзь стоит на восьмом ряду" $
      pieceRow Queen Black `shouldBe` 8

    it "белый король стоит на первом ряду" $
      pieceRow King White `shouldBe` 1

    it "черный король стоит на восьмом ряду" $
      pieceRow King Black `shouldBe` 8

  describe "whitePawns" $ do
    it "возвращает список из 8-ми элементов" $ do
      length whitePawns `shouldBe` 8

    it "первая белая пешка встанет на 'a2'" $ do
      head whitePawns `shouldBe` (('a', 2), Piece Pawn White)

    it "последняя белая пешка встанет на 'h2'" $ do
      last whitePawns `shouldBe` (('h', 2), Piece Pawn White)

  describe "blackPawns" $ do
    it "возвращает список из 8-ми элементов" $ do
      length blackPawns `shouldBe` 8

    it "первая чёрная пешка стоит на 'a7'" $ do
      head blackPawns `shouldBe` (('a', 7), Piece Pawn Black)

    it "последняя чёрная пешка стоит на 'h7'" $ do
      last blackPawns `shouldBe` (('h', 7), Piece Pawn Black)

  describe "whiteRooks" $ do
    it "возвращает список из 2-х элементов" $ do
      length whiteRooks `shouldBe` 2

    it "первая белая ладья встанет на 'a1'" $ do
      head whiteRooks `shouldBe` (('a', 1), Piece Rook White)

    it "вторая белая ладья встанет на 'h1'" $ do
      last whiteRooks `shouldBe` (('h', 1), Piece Rook White)

  describe "blackRooks" $ do
    it "возвращает список из 2-х элементов" $ do
      length blackRooks `shouldBe` 2

    it "первая чёрная ладья встанет на 'a8'" $ do
      head blackRooks `shouldBe` (('a', 8), Piece Rook Black)

    it "вторая чёрная ладья встанет на 'h8'" $ do
      last blackRooks `shouldBe` (('h', 8), Piece Rook Black)

  describe "whiteKnights" $ do
    it "возвращает список из 2-х элементов" $ do
      length whiteKnights `shouldBe` 2

    it "первый белый конь встанет на 'b1'" $ do
      head whiteKnights `shouldBe` (('b', 1), Piece Knight White)

    it "второй белый конь встанет на 'g1'" $ do
      last whiteKnights `shouldBe` (('g', 1), Piece Knight White)

  describe "blackKnights" $ do
    it "возвращает список из 2-х элементов" $ do
      length blackKnights `shouldBe` 2

    it "первый чёрный конь встанет на 'b8'" $ do
      head blackKnights `shouldBe` (('b', 8), Piece Knight Black)

    it "второй чёрный конь встанет на 'g8'" $ do
      last blackKnights `shouldBe` (('g', 8), Piece Knight Black)

  describe "whiteBishops" $ do
    it "возвращает список из 2-х элементов" $ do
      length whiteBishops `shouldBe` 2

    it "первый белый слон встанет на 'c1'" $ do
      head whiteBishops `shouldBe` (('c', 1), Piece Bishop White)

    it "второй белый слон встанет на 'f1'" $ do
      last whiteBishops `shouldBe` (('f', 1), Piece Bishop White)

  describe "blackBishops" $ do
    it "возвращает список из 2-х элементов" $ do
      length blackBishops `shouldBe` 2

    it "первый чёрный слон встанет на 'c8'" $ do
      head blackBishops `shouldBe` (('c', 8), Piece Bishop Black)

    it "второй чёрный слон встанет на 'f8'" $ do
      last blackBishops `shouldBe` (('f', 8), Piece Bishop Black)

  describe "whiteQueen" $ do
    it "белый ферзь встанет на 'd1'" $ do
      whiteQueen `shouldBe` (('d', 1), Piece Queen White)

  describe "blackQueen" $ do
    it "чёрный ферзь встанет на 'd8'" $ do
      blackQueen `shouldBe` (('d', 8), Piece Queen Black)


  describe "whiteKing" $ do
    it "белый король встанет на 'e1'" $ do
      whiteKing `shouldBe` (('e', 1), Piece King White)

  describe "blackKing" $ do
    it "чёрный король встанет на 'a1'" $ do
      blackKing `shouldBe` (('e', 8), Piece King Black)
