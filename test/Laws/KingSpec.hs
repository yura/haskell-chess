module Laws.KingSpec (spec) where

import           Test.Hspec

import qualified Laws
import           Laws.King

import           Board
import           Board.InitialPosition

spec :: Spec
spec = do
  describe "moveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      moveSquares ('h', 6) `shouldBe`
        [
          ('g', 5), ('g', 7)
        , ('g', 6), ('h', 5), ('h', 7)
        ]

  describe "kingValidMoveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      Laws.kingValidMoveSquares White ('h', 4) initialBoard `shouldBe`
        [ ('g', 3), ('g', 5)
        , ('g', 4), ('h', 3), ('h', 5)
        ]

    it "не может приближаться к пешкам" $ do
      let board = movePiece ('e', 1) ('h', 5) kingWhite initialBoard 
      Laws.kingValidMoveSquares White ('h', 5) board `shouldBe`
        [ ('g', 4)
        , ('g', 5), ('h', 4)
        ]

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт король" $ do
      let board = placePiece ('c', 3) pawnBlack initialBoard
      underAttackSquares board White ('b', 2) `shouldBe`
        [ ('c', 3)
        , ('a', 3)
        , ('b', 3)]

  describe "possibleMoves" $ do
    it "все возможные ходы королья на пустой доске" $ do
      let board = placePiece ('d', 4) kingWhite emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move kingWhite ('d', 4) ('c', 3)
        , Move kingWhite ('d', 4) ('e', 3)
        , Move kingWhite ('d', 4) ('e', 5)
        , Move kingWhite ('d', 4) ('c', 5)

        , Move kingWhite ('d', 4) ('c', 4)
        , Move kingWhite ('d', 4) ('d', 3)
        , Move kingWhite ('d', 4) ('e', 4)
        , Move kingWhite ('d', 4) ('d', 5)
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [(('d', 4), kingWhite), (('d', 5), pawnBlack), (('e', 5), pawnBlack)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Capture kingWhite ('d', 4) ('e', 5)
        , Capture kingWhite ('d', 4) ('d', 5)
        , Move kingWhite ('d', 4) ('c', 3)
        , Move kingWhite ('d', 4) ('e', 3)
        , Move kingWhite ('d', 4) ('c', 5)
        , Move kingWhite ('d', 4) ('c', 4)
        , Move kingWhite ('d', 4) ('d', 3)
        , Move kingWhite ('d', 4) ('e', 4)
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [(('d', 4), kingWhite), (('d', 5), pawnWhite), (('e', 5), pawnWhite)] emptyBoard
      possibleMoves board White ('d', 4) `shouldBe`
        [ Move kingWhite ('d', 4) ('c', 3)
        , Move kingWhite ('d', 4) ('e', 3)
        , Move kingWhite ('d', 4) ('c', 5)
        , Move kingWhite ('d', 4) ('c', 4)
        , Move kingWhite ('d', 4) ('d', 3)
        , Move kingWhite ('d', 4) ('e', 4)
        ]

    context "[рокировка]" $ do
      context "[белые]" $ do
        context "[короткая]" $ do
          let board = deletePiece ('f', 1) $ deletePiece ('g', 1) initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board White ('e', 1) `shouldBe` [KingsideCastling White, Move kingWhite ('e',1) ('f',1)] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece ('e', 1) ('d', 1) kingWhite board) White ('d', 1) `shouldBe` [Move kingWhite ('d',1) ('e',1)] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece ('h', 1) ('h', 2) rookWhite board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('f',1)]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling White board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('f',1)]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableKingsideCastling White board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('f',1)]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece ('f', 1) bishopWhite board) White ('e', 1) `shouldBe` []
            possibleMoves (placePiece ('g', 1) bishopWhite board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('f', 1)]

            possibleMoves (placePiece ('g', 1) bishopBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('f', 1)]
            possibleMoves (placePiece ('f', 1) bishopBlack board) White ('e', 1) `shouldBe` [Capture kingWhite ('e', 1) ('f', 1)]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece ('e', 2) rookBlack board) White ('e', 1) `shouldBe` [Capture kingWhite ('e', 1) ('e', 2), Move kingWhite ('e',1) ('f',1)]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece ('g', 2) bishopBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('f',1)]

          it "не рокируется под шах" $ do
            possibleMoves (placePiece ('g', 2) rookBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('f',1)]
        
        context "[длинная]" $ do
          let board = deletePiece ('b', 1) $ deletePiece ('c', 1) $ deletePiece ('d', 1) initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board White ('e', 1) `shouldBe` [QueensideCastling White, Move kingWhite ('e', 1) ('d', 1)] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece ('e', 1) ('f', 1) kingWhite board) White ('f', 1) `shouldBe` [Move kingWhite ('f', 1) ('e', 1)] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece ('a', 1) ('a', 2) rookWhite board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling White board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableQueensideCastling White board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece ('b', 1) bishopWhite board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]
            possibleMoves (placePiece ('c', 1) bishopWhite board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('d',1)]
            possibleMoves (placePiece ('d', 1) bishopWhite board) White ('e', 1) `shouldBe` []

            possibleMoves (placePiece ('b', 1) bishopBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]
            possibleMoves (placePiece ('c', 1) bishopBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e',1) ('d',1)]
            possibleMoves (placePiece ('d', 1) bishopBlack board) White ('e', 1) `shouldBe` [Capture kingWhite ('e',1) ('d',1)]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece ('e', 2) rookBlack board) White ('e', 1) `shouldBe` [Capture kingWhite ('e', 1) ('e', 2), Move kingWhite ('e', 1) ('d', 1)]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece ('c', 2) bishopBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]

          it "не рокируется под шах" $
            possibleMoves (placePiece ('c', 2) rookBlack board) White ('e', 1) `shouldBe` [Move kingWhite ('e', 1) ('d', 1)]

          it "ладья может рокироваться через битое поле" $
            possibleMoves (placePiece ('b', 2) rookBlack board) White ('e', 1) `shouldBe` [QueensideCastling White, Move kingWhite ('e', 1) ('d', 1)]

      context "[чёрные]" $ do
        context "[короткая]" $ do
          let board = deletePiece ('f', 8) $ deletePiece ('g', 8) initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board Black ('e', 8) `shouldBe` [KingsideCastling Black, Move kingBlack ('e',8) ('f',8)] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece ('e', 8) ('d', 8) kingBlack board) Black ('d', 8) `shouldBe` [Move kingBlack ('d', 8) ('e', 8)] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece ('h', 8) ('h', 7) rookBlack board) Black ('e', 8) `shouldBe` [Move kingBlack ('e',8) ('f',8)]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling Black board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('f', 8)]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableKingsideCastling Black board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('f', 8)]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece ('f', 8) bishopBlack board) Black ('e', 8) `shouldBe` []
            possibleMoves (placePiece ('g', 8) bishopBlack board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('f', 8)]

            possibleMoves (placePiece ('g', 8) bishopWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('f', 8)]
            possibleMoves (placePiece ('f', 8) bishopWhite board) Black ('e', 8) `shouldBe` [Capture kingBlack ('e', 8) ('f', 8)]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece ('e', 7) rookWhite board) Black ('e', 8) `shouldBe` [Capture kingBlack ('e', 8) ('e', 7), Move kingBlack ('e', 8) ('f', 8)]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece ('g', 7) bishopWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('f', 8)]

          it "не рокируется под шах" $ do
            possibleMoves (placePiece ('g', 7) rookWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('f', 8)]

        context "[длинная]" $ do
          let board = deletePiece ('b', 8) $ deletePiece ('c', 8) $ deletePiece ('d', 8) initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board Black ('e', 8) `shouldBe` [QueensideCastling Black, Move kingBlack ('e', 8) ('d', 8)] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece ('e', 8) ('f', 8) kingBlack board) Black ('f', 8) `shouldBe` [Move kingBlack ('f', 8) ('e', 8)] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece ('a', 8) ('a', 7) rookBlack board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling Black board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableQueensideCastling Black board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece ('b', 8) bishopBlack board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]
            possibleMoves (placePiece ('c', 8) bishopBlack board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]
            possibleMoves (placePiece ('d', 8) bishopBlack board) Black ('e', 8) `shouldBe` []

            possibleMoves (placePiece ('b', 8) bishopWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]
            possibleMoves (placePiece ('c', 8) bishopWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]
            possibleMoves (placePiece ('d', 8) bishopWhite board) Black ('e', 8) `shouldBe` [Capture kingBlack ('e', 8) ('d', 8)]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece ('e', 7) rookWhite board) Black ('e', 8) `shouldBe` [Capture kingBlack ('e', 8) ('e', 7), Move kingBlack ('e', 8) ('d', 8)]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece ('c', 7) bishopWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]

          it "не рокируется под шах" $
            possibleMoves (placePiece ('c', 7) rookWhite board) Black ('e', 8) `shouldBe` [Move kingBlack ('e', 8) ('d', 8)]

          it "ладья может рокироваться через битое поле" $
            possibleMoves (placePiece ('b', 7) rookWhite board) Black ('e', 8) `shouldBe` [QueensideCastling Black, Move kingBlack ('e', 8) ('d', 8)]
