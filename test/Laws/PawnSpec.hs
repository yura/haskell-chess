module Laws.PawnSpec (spec) where

import Test.Hspec

import qualified Data.Map as M
import           Data.List ( nub )
import qualified Data.Map as Map

import           Board
import           Board.InitialPosition
import           Laws.Pawn

spec :: Spec
spec = do
  describe "underAttackSquares - значит королю на них ходить нельзя" $ do
    context "[белая пешка]" $ do
      it "возращает поля, которые атакует пешка" $
        underAttackSquares initialBoard White (read "e2") `shouldBe` [(read "d3"), (read "f3")] 

      it "что делать со взятием на проходе? - похоже ничего не делать, пропускаем, потому что функция используется для нахождения свободных полей для хода короля" $ pending

    context "[чёрная пешка]" $ do
      it "возращает поля, которые атакует пешка" $
        underAttackSquares initialBoard Black (read "e7") `shouldBe` [(read "d6"), (read "f6")] 

      it "что делать со взятием на проходе?" $ pending

  describe "moveSquares" $ do
    context "[белые ]" $ do
      context "[без взятия фигур противника]" $ do
        it "со своей начальной позиции может ходить на поле вперед или на два поля вперед" $ do
          moveSquares White (read "a2") `shouldBe` [ (read "a3"), (read "a4") ]
          moveSquares White (read "e2") `shouldBe` [ (read "e3"), (read "e4") ]

        it "может ходить на поле вперёд не с начальной позиции" $ do
          moveSquares White (read "b3") `shouldBe` [ (read "b4") ]
          moveSquares White (read "g7") `shouldBe` [ (read "g8") ]

  describe "whitePawnPossibleMoves" $ do
    context "[пешка на начальной позиции]" $ do
      it "ходит вперед и через поле" $ do
        let board = placePiece (read "e2") pawnWhite emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` [Move pawnWhite (read "e2") (read "e3"), Move pawnWhite (read "e2") (read "e4")]

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит своя фигура" $ do
        let board = placePieces [((read "e2"), pawnWhite), ((read "e3"), knightWhite)] emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` []

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит фигура противника" $ do
        let board = placePieces [((read "e2"), pawnWhite), ((read "e3"), knightBlack)] emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` []

      it "не ходит через клетку, если поле занято своей фигурой" $ do
        let board = placePieces [((read "e2"), pawnWhite), ((read "e4"), pawnWhite)] emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` [Move pawnWhite (read "e2") (read "e3")]

      it "не ходит через клетку, если поле занято фигурой противника" $ do
        let board = placePieces [((read "e2"), pawnWhite), ((read "e4"), pawnBlack)] emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` [Move pawnWhite (read "e2") (read "e3")]

      it "рубит фигуру противника" $ do
        let board = placePieces [((read "e2"), pawnWhite), ((read "d3"), pawnBlack)] emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` [Capture pawnWhite (read "e2") (read "d3"), Move pawnWhite (read "e2") (read "e3"), Move pawnWhite (read "e2") (read "e4")]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [((read "f2"), pawnWhite), ((read "g3"), queenBlack), ((read "f3"), pawnBlack)] emptyBoard
        whitePawnPossibleMoves (read "f2") board `shouldBe` [Capture pawnWhite (read "f2") (read "g3")]

      it "не рубит свою фигуру" $ do
        let board = placePieces [((read "e2"), pawnWhite), ((read "d3"), pawnWhite)] emptyBoard
        whitePawnPossibleMoves (read "e2") board `shouldBe` [Move pawnWhite (read "e2") (read "e3"), Move pawnWhite (read "e2") (read "e4")]

      it "не ходит, если король после хода оказывается под шахом" pending
      it "не ходит через поле, если король после хода оказывается под шахом" pending

    context "[пешка не в начальной позиции]" $ do
      it "ходит вперед" $ do
        let board = placePiece (read "e4") pawnWhite emptyBoard
        whitePawnPossibleMoves (read "e4") board `shouldBe` [Move pawnWhite (read "e4") (read "e5")]
        
      it "не ходит вперед, если стоит своя фигура" $ do
        let board = placePieces [((read "e4"), pawnWhite), ((read "e5"), knightWhite)] emptyBoard
        whitePawnPossibleMoves (read "e4") board `shouldBe` []

      it "не ходит вперед, если стоит фигура противника" $ do
        let board = placePieces [((read "e4"), pawnWhite), ((read "e5"), knightBlack)] emptyBoard
        whitePawnPossibleMoves (read "e4") board `shouldBe` []

      it "рубит фигуру противника" $ do
        let board = placePieces [((read "e4"), pawnWhite), ((read "d5"), pawnBlack)] emptyBoard
        whitePawnPossibleMoves (read "e4") board `shouldBe` [Capture pawnWhite (read "e4") (read "d5"), Move pawnWhite (read "e4") (read "e5")]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [((read "f4"), pawnWhite), ((read "e5"), queenBlack), ((read "f5"), pawnBlack)] emptyBoard
        whitePawnPossibleMoves (read "f4") board `shouldBe` [Capture pawnWhite (read "f4") (read "e5")]

      it "не рубит свою фигуру" $ do
        let board = placePieces [((read "e4"), pawnWhite), ((read "d5"), pawnWhite)] emptyBoard
        whitePawnPossibleMoves (read "e4") board `shouldBe` [Move pawnWhite (read "e4") (read "e5")]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "взятие на проходе" $ do
      it "рубит пешку противника на проходе" $ do
        let board = move (placePiece (read "e5") pawnWhite emptyBoard) (Move pawnBlack (read "f7") (read "f5")) 
        whitePawnPossibleMoves (read "e5") board `shouldBe` [EnPassantCapture pawnWhite (read "e5") (read "f6"), Move pawnWhite (read "e5") (read "e6")]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "[продвижение]" $ do
      it "продвигается до ферзя, ладьи, слона или коня" $ do
        let board = placePiece (read "e7") pawnWhite emptyBoard
        whitePawnPossibleMoves (read "e7") board `shouldBe`
          [ Promotion (read "e7") (read "e8") queenWhite
          , Promotion (read "e7") (read "e8") rookWhite
          , Promotion (read "e7") (read "e8") bishopWhite
          , Promotion (read "e7") (read "e8") knightWhite
          ]
        
      it "рубит фигуры противника с последующим продвижением" $ do
        let board = placePieces [((read "e7"), pawnWhite), ((read "d8"), queenBlack)] emptyBoard
        whitePawnPossibleMoves (read "e7") board `shouldBe`
          [ CapturePromotion (read "e7") (read "d8") queenWhite
          , CapturePromotion (read "e7") (read "d8") rookWhite
          , CapturePromotion (read "e7") (read "d8") bishopWhite
          , CapturePromotion (read "e7") (read "d8") knightWhite
          , Promotion (read "e7") (read "e8") queenWhite
          , Promotion (read "e7") (read "e8") rookWhite
          , Promotion (read "e7") (read "e8") bishopWhite
          , Promotion (read "e7") (read "e8") knightWhite
          ]
 
      it "не рубит свои фигуры с последующим продвижением" $ do
        let board = placePieces [((read "e7"), pawnWhite), ((read "d8"), queenWhite)] emptyBoard
        whitePawnPossibleMoves (read "e7") board `shouldBe`
          [ Promotion (read "e7") (read "e8") queenWhite
          , Promotion (read "e7") (read "e8") rookWhite
          , Promotion (read "e7") (read "e8") bishopWhite
          , Promotion (read "e7") (read "e8") knightWhite
          ]

      it "не ходит, если король после хода оказывается под шахом" pending

  describe "moveSquares Black" $ do
    context "[без взятия фигур противника]" $ do
      it "со своей начальной позиции может ходить на поле вперед или на два поля вперед" $ do
        moveSquares Black (read "a7") `shouldBe` [ (read "a6"), (read "a5") ]
        moveSquares Black (read "e7") `shouldBe` [ (read "e6"), (read "e5") ]

      it "может ходить на поле вперёд не с начальной позиции" $ do
        moveSquares Black (read "b3") `shouldBe` [ (read "b2") ]
        moveSquares Black (read "g2") `shouldBe` [ (read "g1") ]

  describe "blackPawnPossibleMoves" $ do
    context "[пешка на начальной позиции]" $ do
      it "ходит вперед и через поле" $ do
        let board = placePiece (read "f7") pawnBlack emptyBoard
        blackPawnPossibleMoves (read "f7") board `shouldBe` [Move pawnBlack (read "f7") (read "f6"), Move pawnBlack (read "f7") (read "f5")]

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит своя фигура" $ do
        let board = placePieces [((read "e7"), pawnBlack), ((read "e6"), knightBlack)] emptyBoard
        blackPawnPossibleMoves (read "e7") board `shouldBe` []

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит фигура противника" $ do
        let board = placePieces [((read "g7"), pawnBlack), ((read "g6"), knightWhite)] emptyBoard
        blackPawnPossibleMoves (read "g7") board `shouldBe` []

      it "не ходит через клетку, если поле занято своей фигурой" $ do
        let board = placePieces [((read "h7"), pawnBlack), ((read "h5"), bishopBlack)] emptyBoard
        blackPawnPossibleMoves (read "h7") board `shouldBe` [Move pawnBlack (read "h7") (read "h6")]

      it "не ходит через клетку, если поле занято фигурой противника" $ do
        let board = placePieces [((read "a7"), pawnBlack), ((read "a5"), queenWhite)] emptyBoard
        blackPawnPossibleMoves (read "a7") board `shouldBe` [Move pawnBlack (read "a7") (read "a6")]

      it "рубит фигуру противника" $ do
        let board = placePieces [((read "b7"), pawnBlack), ((read "c6"), pawnWhite)] emptyBoard
        blackPawnPossibleMoves (read "b7") board `shouldBe` [Capture pawnBlack (read "b7") (read "c6"), Move pawnBlack (read "b7") (read "b6"), Move pawnBlack (read "b7") (read "b5")]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [((read "f7"), pawnBlack), ((read "e6"), queenWhite), ((read "f6"), pawnBlack)] emptyBoard
        blackPawnPossibleMoves (read "f7") board `shouldBe` [Capture pawnBlack (read "f7") (read "e6")]

      it "не рубит свою фигуру" $ do
        let board = placePieces [((read "b7"), pawnBlack), ((read "c6"), pawnBlack)] emptyBoard
        blackPawnPossibleMoves (read "b7") board `shouldBe` [Move pawnBlack (read "b7") (read "b6"), Move pawnBlack (read "b7") (read "b5")]

      it "не ходит, если король после хода оказывается под шахом" pending
      it "не ходит через поле, если король после хода оказывается под шахом" pending

    context "[пешка не в начальной позиции]" $ do
      it "ходит вперед" $ do
        let board = placePiece (read "c4") pawnBlack emptyBoard
        blackPawnPossibleMoves (read "c4") board `shouldBe` [Move pawnBlack (read "c4") (read "c3")]
        
      it "не ходит вперед, если стоит своя фигура" $ do
        let board = placePieces [((read "d4"), pawnBlack), ((read "d3"), knightBlack)] emptyBoard
        blackPawnPossibleMoves (read "d4") board `shouldBe` []

      it "не ходит вперед, если стоит фигура противника" $ do
        let board = placePieces [((read "e4"), pawnBlack), ((read "e3"), kingWhite)] emptyBoard
        blackPawnPossibleMoves (read "e4") board `shouldBe` []

      it "рубит фигуру противника" $ do
        let board = placePieces [((read "f4"), pawnBlack), ((read "g3"), queenWhite)] emptyBoard
        blackPawnPossibleMoves (read "f4") board `shouldBe` [Capture pawnBlack (read "f4") (read "g3"), Move pawnBlack (read "f4") (read "f3")]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [((read "f4"), pawnBlack), ((read "g3"), queenWhite), ((read "f3"), pawnBlack)] emptyBoard
        blackPawnPossibleMoves (read "f4") board `shouldBe` [Capture pawnBlack (read "f4") (read "g3")]

      it "не рубит свою фигуру" $ do
        let board = placePieces [((read "e4"), pawnBlack), ((read "d3"), pawnBlack)] emptyBoard
        blackPawnPossibleMoves (read "e4") board `shouldBe` [Move pawnBlack (read "e4") (read "e3")]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "взятие на проходе" $ do
      it "рубит пешку противника на проходе" $ do
        let board = move (placePiece (read "e4") pawnBlack emptyBoard) (Move pawnWhite (read "d2") (read "d4")) 
        blackPawnPossibleMoves (read "e4") board `shouldBe` [EnPassantCapture pawnBlack (read "e4") (read "d3"), Move pawnBlack (read "e4") (read "e3")]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "[продвижение]" $ do
      it "продвигается до ферзя, ладьи, слона или коня" $ do
        let board = placePiece (read "e2") pawnBlack emptyBoard
        blackPawnPossibleMoves (read "e2") board `shouldBe`
          [ Promotion (read "e2") (read "e1") queenBlack
          , Promotion (read "e2") (read "e1") rookBlack
          , Promotion (read "e2") (read "e1") bishopBlack
          , Promotion (read "e2") (read "e1") knightBlack
          ]
        
      it "рубит фигуры противника с последующим продвижением" $ do
        let board = placePieces [((read "e2"), pawnBlack), ((read "f1"), queenWhite)] emptyBoard
        blackPawnPossibleMoves (read "e2") board `shouldBe`
          [ CapturePromotion (read "e2") (read "f1") queenBlack
          , CapturePromotion (read "e2") (read "f1") rookBlack
          , CapturePromotion (read "e2") (read "f1") bishopBlack
          , CapturePromotion (read "e2") (read "f1") knightBlack
          , Promotion (read "e2") (read "e1") queenBlack
          , Promotion (read "e2") (read "e1") rookBlack
          , Promotion (read "e2") (read "e1") bishopBlack
          , Promotion (read "e2") (read "e1") knightBlack
          ]
 
      it "не рубит свои фигуры с последующим продвижением" $ do
        let board = placePieces [((read "e2"), pawnBlack), ((read "d8"), queenBlack)] emptyBoard
        blackPawnPossibleMoves (read "e2") board `shouldBe`
          [ Promotion (read "e2") (read "e1") queenBlack
          , Promotion (read "e2") (read "e1") rookBlack
          , Promotion (read "e2") (read "e1") bishopBlack
          , Promotion (read "e2") (read "e1") knightBlack
          ]

      it "не ходит, если король после хода оказывается под шахом" pending

