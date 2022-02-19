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
        underAttackSquares initialBoard White ('e', 2) `shouldBe` [('d', 3), ('f', 3)] 

      it "что делать со взятием на проходе? - похоже ничего не делать, пропускаем, потому что функция используется для нахождения свободных полей для хода короля" $ pending

    context "[чёрная пешка]" $ do
      it "возращает поля, которые атакует пешка" $
        underAttackSquares initialBoard Black ('e', 7) `shouldBe` [('d', 6), ('f', 6)] 

      it "что делать со взятием на проходе?" $ pending

  describe "whitePawnMoveSquares" $ do
    context "[без взятия фигур противника]" $ do
      it "со своей начальной позиции может ходить на поле вперед или на два поля вперед" $ do
        whitePawnMoveSquares ('a', 2) `shouldBe` [ ('a', 3), ('a', 4) ]
        whitePawnMoveSquares ('e', 2) `shouldBe` [ ('e', 3), ('e', 4) ]

      it "может ходить на поле вперёд не с начальной позиции" $ do
        whitePawnMoveSquares ('b', 3) `shouldBe` [ ('b', 4) ]
        whitePawnMoveSquares ('g', 7) `shouldBe` [ ('g', 8) ]

  describe "whitePawnPossibleMoves" $ do
    context "[пешка на начальной позиции]" $ do
      it "ходит вперед и через поле" $ do
        let board = placePiece ('e', 2) pawnWhite emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3), Move pawnWhite ('e', 2) ('e', 4)]

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит своя фигура" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 3), knightWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` []

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит фигура противника" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 3), knightBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` []

      it "не ходит через клетку, если поле занято своей фигурой" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 4), pawnWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3)]

      it "не ходит через клетку, если поле занято фигурой противника" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('e', 4), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3)]

      it "рубит фигуру противника" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('d', 3), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Capture pawnWhite ('e', 2) ('d', 3), Move pawnWhite ('e', 2) ('e', 3), Move pawnWhite ('e', 2) ('e', 4)]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [(('f', 2), pawnWhite), (('g', 3), queenBlack), (('f', 3), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('f', 2) board `shouldBe` [Capture pawnWhite ('f', 2) ('g', 3)]

      it "не рубит свою фигуру" $ do
        let board = placePieces [(('e', 2), pawnWhite), (('d', 3), pawnWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 2) board `shouldBe` [Move pawnWhite ('e', 2) ('e', 3), Move pawnWhite ('e', 2) ('e', 4)]

      it "не ходит, если король после хода оказывается под шахом" pending
      it "не ходит через поле, если король после хода оказывается под шахом" pending

    context "[пешка не в начальной позиции]" $ do
      it "ходит вперед" $ do
        let board = placePiece ('e', 4) pawnWhite emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Move pawnWhite ('e', 4) ('e', 5)]
        
      it "не ходит вперед, если стоит своя фигура" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('e', 5), knightWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` []

      it "не ходит вперед, если стоит фигура противника" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('e', 5), knightBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` []

      it "рубит фигуру противника" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('d', 5), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Capture pawnWhite ('e', 4) ('d', 5), Move pawnWhite ('e', 4) ('e', 5)]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [(('f', 4), pawnWhite), (('e', 5), queenBlack), (('f', 5), pawnBlack)] emptyBoard
        whitePawnPossibleMoves ('f', 4) board `shouldBe` [Capture pawnWhite ('f', 4) ('e', 5)]

      it "не рубит свою фигуру" $ do
        let board = placePieces [(('e', 4), pawnWhite), (('d', 5), pawnWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 4) board `shouldBe` [Move pawnWhite ('e', 4) ('e', 5)]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "взятие на проходе" $ do
      it "рубит пешку противника на проходе" $ do
        let board = move (placePiece ('e', 5) pawnWhite emptyBoard) (Move pawnBlack ('f', 7) ('f', 5)) 
        whitePawnPossibleMoves ('e', 5) board `shouldBe` [EnPassantCapture pawnWhite ('e', 5) ('f', 6), Move pawnWhite ('e', 5) ('e', 6)]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "[продвижение]" $ do
      it "продвигается до ферзя, ладьи, слона или коня" $ do
        let board = placePiece ('e', 7) pawnWhite emptyBoard
        whitePawnPossibleMoves ('e', 7) board `shouldBe`
          [ Promotion ('e', 7) ('e', 8) queenWhite
          , Promotion ('e', 7) ('e', 8) rookWhite
          , Promotion ('e', 7) ('e', 8) bishopWhite
          , Promotion ('e', 7) ('e', 8) knightWhite
          ]
        
      it "рубит фигуры противника с последующим продвижением" $ do
        let board = placePieces [(('e', 7), pawnWhite), (('d', 8), queenBlack)] emptyBoard
        whitePawnPossibleMoves ('e', 7) board `shouldBe`
          [ CapturePromotion ('e', 7) ('d', 8) queenWhite
          , CapturePromotion ('e', 7) ('d', 8) rookWhite
          , CapturePromotion ('e', 7) ('d', 8) bishopWhite
          , CapturePromotion ('e', 7) ('d', 8) knightWhite
          , Promotion ('e', 7) ('e', 8) queenWhite
          , Promotion ('e', 7) ('e', 8) rookWhite
          , Promotion ('e', 7) ('e', 8) bishopWhite
          , Promotion ('e', 7) ('e', 8) knightWhite
          ]
 
      it "не рубит свои фигуры с последующим продвижением" $ do
        let board = placePieces [(('e', 7), pawnWhite), (('d', 8), queenWhite)] emptyBoard
        whitePawnPossibleMoves ('e', 7) board `shouldBe`
          [ Promotion ('e', 7) ('e', 8) queenWhite
          , Promotion ('e', 7) ('e', 8) rookWhite
          , Promotion ('e', 7) ('e', 8) bishopWhite
          , Promotion ('e', 7) ('e', 8) knightWhite
          ]

      it "не ходит, если король после хода оказывается под шахом" pending

  describe "blackPawnMoveSquares" $ do
    context "[без взятия фигур противника]" $ do
      it "со своей начальной позиции может ходить на поле вперед или на два поля вперед" $ do
        blackPawnMoveSquares ('a', 7) `shouldBe` [ ('a', 6), ('a', 5) ]
        blackPawnMoveSquares ('e', 7) `shouldBe` [ ('e', 6), ('e', 5) ]

      it "может ходить на поле вперёд не с начальной позиции" $ do
        blackPawnMoveSquares ('b', 3) `shouldBe` [ ('b', 2) ]
        blackPawnMoveSquares ('g', 2) `shouldBe` [ ('g', 1) ]

  describe "blackPawnPossibleMoves" $ do
    context "[пешка на начальной позиции]" $ do
      it "ходит вперед и через поле" $ do
        let board = placePiece ('f', 7) pawnBlack emptyBoard
        blackPawnPossibleMoves ('f', 7) board `shouldBe` [Move pawnBlack ('f', 7) ('f', 6), Move pawnBlack ('f', 7) ('f', 5)]

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит своя фигура" $ do
        let board = placePieces [(('e', 7), pawnBlack), (('e', 6), knightBlack)] emptyBoard
        blackPawnPossibleMoves ('e', 7) board `shouldBe` []

      it "не ходит вперед и не перепрыгивает через фигуру, если стоит фигура противника" $ do
        let board = placePieces [(('g', 7), pawnBlack), (('g', 6), knightWhite)] emptyBoard
        blackPawnPossibleMoves ('g', 7) board `shouldBe` []

      it "не ходит через клетку, если поле занято своей фигурой" $ do
        let board = placePieces [(('h', 7), pawnBlack), (('h', 5), bishopBlack)] emptyBoard
        blackPawnPossibleMoves ('h', 7) board `shouldBe` [Move pawnBlack ('h', 7) ('h', 6)]

      it "не ходит через клетку, если поле занято фигурой противника" $ do
        let board = placePieces [(('a', 7), pawnBlack), (('a', 5), queenWhite)] emptyBoard
        blackPawnPossibleMoves ('a', 7) board `shouldBe` [Move pawnBlack ('a', 7) ('a', 6)]

      it "рубит фигуру противника" $ do
        let board = placePieces [(('b', 7), pawnBlack), (('c', 6), pawnWhite)] emptyBoard
        blackPawnPossibleMoves ('b', 7) board `shouldBe` [Capture pawnBlack ('b', 7) ('c', 6), Move pawnBlack ('b', 7) ('b', 6), Move pawnBlack ('b', 7) ('b', 5)]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [(('f', 7), pawnBlack), (('e', 6), queenWhite), (('f', 6), pawnBlack)] emptyBoard
        blackPawnPossibleMoves ('f', 7) board `shouldBe` [Capture pawnBlack ('f', 7) ('e', 6)]

      it "не рубит свою фигуру" $ do
        let board = placePieces [(('b', 7), pawnBlack), (('c', 6), pawnBlack)] emptyBoard
        blackPawnPossibleMoves ('b', 7) board `shouldBe` [Move pawnBlack ('b', 7) ('b', 6), Move pawnBlack ('b', 7) ('b', 5)]

      it "не ходит, если король после хода оказывается под шахом" pending
      it "не ходит через поле, если король после хода оказывается под шахом" pending

    context "[пешка не в начальной позиции]" $ do
      it "ходит вперед" $ do
        let board = placePiece ('c', 4) pawnBlack emptyBoard
        blackPawnPossibleMoves ('c', 4) board `shouldBe` [Move pawnBlack ('c', 4) ('c', 3)]
        
      it "не ходит вперед, если стоит своя фигура" $ do
        let board = placePieces [(('d', 4), pawnBlack), (('d', 3), knightBlack)] emptyBoard
        blackPawnPossibleMoves ('d', 4) board `shouldBe` []

      it "не ходит вперед, если стоит фигура противника" $ do
        let board = placePieces [(('e', 4), pawnBlack), (('e', 3), kingWhite)] emptyBoard
        blackPawnPossibleMoves ('e', 4) board `shouldBe` []

      it "рубит фигуру противника" $ do
        let board = placePieces [(('f', 4), pawnBlack), (('g', 3), queenWhite)] emptyBoard
        blackPawnPossibleMoves ('f', 4) board `shouldBe` [Capture pawnBlack ('f', 4) ('g', 3), Move pawnBlack ('f', 4) ('f', 3)]

      it "рубит фигуру противника, даже если хода вперёд нет" $ do
        let board = placePieces [(('f', 4), pawnBlack), (('g', 3), queenWhite), (('f', 3), pawnBlack)] emptyBoard
        blackPawnPossibleMoves ('f', 4) board `shouldBe` [Capture pawnBlack ('f', 4) ('g', 3)]

      it "не рубит свою фигуру" $ do
        let board = placePieces [(('e', 4), pawnBlack), (('d', 3), pawnBlack)] emptyBoard
        blackPawnPossibleMoves ('e', 4) board `shouldBe` [Move pawnBlack ('e', 4) ('e', 3)]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "взятие на проходе" $ do
      it "рубит пешку противника на проходе" $ do
        let board = move (placePiece ('e', 4) pawnBlack emptyBoard) (Move pawnWhite ('d', 2) ('d', 4)) 
        blackPawnPossibleMoves ('e', 4) board `shouldBe` [EnPassantCapture pawnBlack ('e', 4) ('d', 3), Move pawnBlack ('e', 4) ('e', 3)]

      it "не ходит, если король после хода оказывается под шахом" pending

    context "[продвижение]" $ do
      it "продвигается до ферзя, ладьи, слона или коня" $ do
        let board = placePiece ('e', 2) pawnBlack emptyBoard
        blackPawnPossibleMoves ('e', 2) board `shouldBe`
          [ Promotion ('e', 2) ('e', 1) queenBlack
          , Promotion ('e', 2) ('e', 1) rookBlack
          , Promotion ('e', 2) ('e', 1) bishopBlack
          , Promotion ('e', 2) ('e', 1) knightBlack
          ]
        
      it "рубит фигуры противника с последующим продвижением" $ do
        let board = placePieces [(('e', 2), pawnBlack), (('f', 1), queenWhite)] emptyBoard
        blackPawnPossibleMoves ('e', 2) board `shouldBe`
          [ CapturePromotion ('e', 2) ('f', 1) queenBlack
          , CapturePromotion ('e', 2) ('f', 1) rookBlack
          , CapturePromotion ('e', 2) ('f', 1) bishopBlack
          , CapturePromotion ('e', 2) ('f', 1) knightBlack
          , Promotion ('e', 2) ('e', 1) queenBlack
          , Promotion ('e', 2) ('e', 1) rookBlack
          , Promotion ('e', 2) ('e', 1) bishopBlack
          , Promotion ('e', 2) ('e', 1) knightBlack
          ]
 
      it "не рубит свои фигуры с последующим продвижением" $ do
        let board = placePieces [(('e', 2), pawnBlack), (('d', 8), queenBlack)] emptyBoard
        blackPawnPossibleMoves ('e', 2) board `shouldBe`
          [ Promotion ('e', 2) ('e', 1) queenBlack
          , Promotion ('e', 2) ('e', 1) rookBlack
          , Promotion ('e', 2) ('e', 1) bishopBlack
          , Promotion ('e', 2) ('e', 1) knightBlack
          ]

      it "не ходит, если король после хода оказывается под шахом" pending

