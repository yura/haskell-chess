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
      moveSquares (read "h6") `shouldBe`
        [
          (read "g5"), (read "g7")
        , (read "g6"), (read "h5"), (read "h7")
        ]

  describe "kingValidMoveSquares" $ do
    it "все ходы короля, если король находится в центре" $ do
      Laws.kingValidMoveSquares White (read "h4") initialBoard `shouldBe`
        [ (read "g3"), (read "g5")
        , (read "g4"), (read "h3"), (read "h5")
        ]

    it "не может приближаться к пешкам" $ do
      let board = movePiece (read "e1") (read "h5") kingWhite initialBoard 
      Laws.kingValidMoveSquares White (read "h5") board `shouldBe`
        [ (read "g4")
        , (read "g5"), (read "h4")
        ]

  describe "underAttackSquares" $ do
    it "все поля, которые бьёт король" $ do
      let board = placePiece (read "c3") pawnBlack initialBoard
      underAttackSquares board White (read "b2") `shouldBe`
        [ (read "c3")
        , (read "a3")
        , (read "b3")]

  describe "possibleMoves" $ do
    it "все возможные ходы королья на пустой доске" $ do
      let board = placePiece (read "d4") kingWhite emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move kingWhite (read "d4") (read "c3")
        , Move kingWhite (read "d4") (read "e3")
        , Move kingWhite (read "d4") (read "e5")
        , Move kingWhite (read "d4") (read "c5")

        , Move kingWhite (read "d4") (read "c4")
        , Move kingWhite (read "d4") (read "d3")
        , Move kingWhite (read "d4") (read "e4")
        , Move kingWhite (read "d4") (read "d5")
        ]

    it "взятие фигур соперника" $ do
      let board = placePieces [((read "d4"), kingWhite), ((read "d5"), pawnBlack), ((read "e5"), pawnBlack)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Capture kingWhite (read "d4") (read "e5")
        , Capture kingWhite (read "d4") (read "d5")
        , Move kingWhite (read "d4") (read "c3")
        , Move kingWhite (read "d4") (read "e3")
        , Move kingWhite (read "d4") (read "c5")
        , Move kingWhite (read "d4") (read "c4")
        , Move kingWhite (read "d4") (read "d3")
        , Move kingWhite (read "d4") (read "e4")
        ]      
 
    it "не может ходить на поля, которые заняты своими фигурами" $ do
      let board = placePieces [((read "d4"), kingWhite), ((read "d5"), pawnWhite), ((read "e5"), pawnWhite)] emptyBoard
      possibleMoves board White (read "d4") `shouldBe`
        [ Move kingWhite (read "d4") (read "c3")
        , Move kingWhite (read "d4") (read "e3")
        , Move kingWhite (read "d4") (read "c5")
        , Move kingWhite (read "d4") (read "c4")
        , Move kingWhite (read "d4") (read "d3")
        , Move kingWhite (read "d4") (read "e4")
        ]

    context "[рокировка]" $ do
      context "[белые]" $ do
        context "[короткая]" $ do
          let board = deletePiece (read "f1") $ deletePiece (read "g1") initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board White (read "e1") `shouldBe` [KingsideCastling White, Move kingWhite (read "e1") (read "f1")] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece (read "e1") (read "d1") kingWhite board) White (read "d1") `shouldBe` [Move kingWhite (read "d1") (read "e1")] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece (read "h1") (read "h2") rookWhite board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling White board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableKingsideCastling White board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece (read "f1") bishopWhite board) White (read "e1") `shouldBe` []
            possibleMoves (placePiece (read "g1") bishopWhite board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]

            possibleMoves (placePiece (read "g1") bishopBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]
            possibleMoves (placePiece (read "f1") bishopBlack board) White (read "e1") `shouldBe` [Capture kingWhite (read "e1") (read "f1")]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece (read "e2") rookBlack board) White (read "e1") `shouldBe` [Capture kingWhite (read "e1") (read "e2"), Move kingWhite (read "e1") (read "f1")]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece (read "g2") bishopBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]

          it "не рокируется под шах" $ do
            possibleMoves (placePiece (read "g2") rookBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "f1")]
        
        context "[длинная]" $ do
          let board = deletePiece (read "b1") $ deletePiece (read "c1") $ deletePiece (read "d1") initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board White (read "e1") `shouldBe` [QueensideCastling White, Move kingWhite (read "e1") (read "d1")] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece (read "e1") (read "f1") kingWhite board) White (read "f1") `shouldBe` [Move kingWhite (read "f1") (read "e1")] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece (read "a1") (read "a2") rookWhite board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling White board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableQueensideCastling White board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece (read "b1") bishopWhite board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]
            possibleMoves (placePiece (read "c1") bishopWhite board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]
            possibleMoves (placePiece (read "d1") bishopWhite board) White (read "e1") `shouldBe` []

            possibleMoves (placePiece (read "b1") bishopBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]
            possibleMoves (placePiece (read "c1") bishopBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]
            possibleMoves (placePiece (read "d1") bishopBlack board) White (read "e1") `shouldBe` [Capture kingWhite (read "e1") (read "d1")]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece (read "e2") rookBlack board) White (read "e1") `shouldBe` [Capture kingWhite (read "e1") (read "e2"), Move kingWhite (read "e1") (read "d1")]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece (read "c2") bishopBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]

          it "не рокируется под шах" $
            possibleMoves (placePiece (read "c2") rookBlack board) White (read "e1") `shouldBe` [Move kingWhite (read "e1") (read "d1")]

          it "ладья может рокироваться через битое поле" $
            possibleMoves (placePiece (read "b2") rookBlack board) White (read "e1") `shouldBe` [QueensideCastling White, Move kingWhite (read "e1") (read "d1")]

      context "[чёрные]" $ do
        context "[короткая]" $ do
          let board = deletePiece (read "f8") $ deletePiece (read "g8") initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board Black (read "e8") `shouldBe` [KingsideCastling Black, Move kingBlack (read "e8") (read "f8")] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece (read "e8") (read "d8") kingBlack board) Black (read "d8") `shouldBe` [Move kingBlack (read "d8") (read "e8")] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece (read "h8") (read "h7") rookBlack board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling Black board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableKingsideCastling Black board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece (read "f8") bishopBlack board) Black (read "e8") `shouldBe` []
            possibleMoves (placePiece (read "g8") bishopBlack board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]

            possibleMoves (placePiece (read "g8") bishopWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]
            possibleMoves (placePiece (read "f8") bishopWhite board) Black (read "e8") `shouldBe` [Capture kingBlack (read "e8") (read "f8")]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece (read "e7") rookWhite board) Black (read "e8") `shouldBe` [Capture kingBlack (read "e8") (read "e7"), Move kingBlack (read "e8") (read "f8")]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece (read "g7") bishopWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]

          it "не рокируется под шах" $ do
            possibleMoves (placePiece (read "g7") rookWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "f8")]

        context "[длинная]" $ do
          let board = deletePiece (read "b8") $ deletePiece (read "c8") $ deletePiece (read "d8") initialBoard

          it "рокируются, если нет фигур между королём и ладьёй, король не под шахом, проходит через небитое поле, встаёт не под шах, раньше не ходил ни король, ни ладья" $ do
            possibleMoves board Black (read "e8") `shouldBe` [QueensideCastling Black, Move kingBlack (read "e8") (read "d8")] 

          it "не рокируется, если король не в начальной позиции" $ do
            possibleMoves (movePiece (read "e8") (read "f8") kingBlack board) Black (read "f8") `shouldBe` [Move kingBlack (read "f8") (read "e8")] 

          it "не рокируется, если ладья не в начальной позиции" $ do
            possibleMoves (movePiece (read "a8") (read "a7") rookBlack board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]

          it "не рокируется, если король ходил" $
            possibleMoves (disableCastling Black board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]

          it "не рокируется, если ладья ходила" $
            possibleMoves (disableQueensideCastling Black board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]

          it "не рокируется, если между королём и ладьёй стоит фигура" $ do
            possibleMoves (placePiece (read "b8") bishopBlack board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]
            possibleMoves (placePiece (read "c8") bishopBlack board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]
            possibleMoves (placePiece (read "d8") bishopBlack board) Black (read "e8") `shouldBe` []

            possibleMoves (placePiece (read "b8") bishopWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]
            possibleMoves (placePiece (read "c8") bishopWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]
            possibleMoves (placePiece (read "d8") bishopWhite board) Black (read "e8") `shouldBe` [Capture kingBlack (read "e8") (read "d8")]

          it "не рокируется, если король под шахом" $
            possibleMoves (placePiece (read "e7") rookWhite board) Black (read "e8") `shouldBe` [Capture kingBlack (read "e8") (read "e7"), Move kingBlack (read "e8") (read "d8")]

          it "не рокируется, если король пройдёт через битое поле" $
            possibleMoves (placePiece (read "c7") bishopWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]

          it "не рокируется под шах" $
            possibleMoves (placePiece (read "c7") rookWhite board) Black (read "e8") `shouldBe` [Move kingBlack (read "e8") (read "d8")]

          it "ладья может рокироваться через битое поле" $
            possibleMoves (placePiece (read "b7") rookWhite board) Black (read "e8") `shouldBe` [QueensideCastling Black, Move kingBlack (read "e8") (read "d8")]
