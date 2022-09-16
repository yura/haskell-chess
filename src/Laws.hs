module Laws where

import           Data.List (nub)
import qualified Data.Map as Map
import           Data.Maybe (mapMaybe)

import           Board
import qualified Laws.Pawn   as P
import qualified Laws.Knight as N
import qualified Laws.Bishop as B
import qualified Laws.Rook   as R
import qualified Laws.Queen  as Q
import qualified Laws.King   as K
import           Laws.Util
import Board (DrawType(ThreefoldRepetition))

possibleMoves :: Board -> [Move]
possibleMoves board@Board{..} = foldl (movesOfPiecesByColor nextMove) [] $ zip [0..] squares
  where
    movesOfPiecesByColor :: Color -> [Move] -> (Int, Maybe Piece) -> [Move]
    movesOfPiecesByColor _     result (_, Nothing)             = result
    movesOfPiecesByColor color result (i, Just p@(Piece pt c)) | c == color = (piecePossibleMoves i p board) ++ result
                                                               | otherwise  = result

piecePossibleMoves :: Int -> Piece -> Board -> [Move]
piecePossibleMoves index p@(Piece Pawn c) b = pawnValidMoves b c (indexToSquare index)
piecePossibleMoves index p@(Piece Knight c) b = knightValidMoves b c (indexToSquare index)
piecePossibleMoves index p@(Piece Bishop c) b = bishopValidMoves b c (indexToSquare index)
piecePossibleMoves index p@(Piece Rook c) b = rookValidMoves b c (indexToSquare index)
piecePossibleMoves index p@(Piece Queen c) b = queenValidMoves b c (indexToSquare index)
piecePossibleMoves index p@(Piece King c) b = kingValidMoves b c (indexToSquare index)

-- Фигуры соперника, которые находятся под угрозой взятия.
captureThreatSquares :: Piece -> Square -> Board -> [Square]
captureThreatSquares (Piece Pawn color)   square board = P.captureThreatSquares color square board
captureThreatSquares (Piece Knight color) square board = N.captureThreatSquares color square board
captureThreatSquares (Piece Bishop color) square board = B.captureThreatSquares color square board
captureThreatSquares (Piece Rook color)   square board = R.captureThreatSquares color square board
captureThreatSquares (Piece Queen color)  square board = Q.captureThreatSquares color square board
-- Так как короли не могут приближаться друг к другу, то
-- ситуация, когда один король шахует другого невозможна. 
-- Иначе будет бесконечный цикл.
captureThreatSquares (Piece King color)   square board = kingCaptureThreatSquares color square board -- K.captureThreatSquares color square board

allCaptureThreatSquares :: Color -> Board -> [Square]
allCaptureThreatSquares color board@Board{..}
  = nub $ foldl threatsByColor [] $ zip [0..] squares
  where
    threatsByColor result (_, Nothing)                = result
    threatsByColor result (index, Just p@(Piece _ c)) | c == color = result ++ captureThreatSquares p (indexToSquare index) board
                                                      | otherwise  = result

-- Поля, которые находятся под угрозой шаха. Если король
-- соперника находится на одном из этих полей, то ему шах
-- и ходить на эти поля королю нельзя.
checkThreatSquares :: Piece -> Square -> Board -> [Square]
checkThreatSquares (Piece Pawn color)   square board = P.underAttackSquares board color square
checkThreatSquares (Piece Knight color) square _     = N.underAttackSquares square
checkThreatSquares (Piece Bishop color) square board = B.underAttackSquares board color square
checkThreatSquares (Piece Rook color)   square board = R.underAttackSquares board color square
checkThreatSquares (Piece Queen color)  square board = Q.underAttackSquares board color square
-- Так как короли не могут приближаться друг к другу, то
-- ситуация, когда один король шахует другого невозможна. 
-- Иначе будет бесконечный цикл.
checkThreatSquares (Piece King color)   square _     = K.moveSquares square

allCheckThreatSquares :: Color -> Board -> [Square]
allCheckThreatSquares color board@(Board{..})
  = nub $ concatMap (\(square, piece) -> checkThreatSquares piece square board) $ pieces color board

-- Допустимые ходы в соответствии с правилами:
pawnValidMoves :: Board -> Color -> Square -> [Move]
pawnValidMoves board color square = filter (not . isCheck color . move board) $ P.pawnPossibleMoves color square board

knightValidMoves :: Board -> Color -> Square -> [Move]
knightValidMoves board color square = filter (not . isCheck color . move board) $ N.possibleMoves board color square 

bishopValidMoves :: Board -> Color -> Square -> [Move]
bishopValidMoves board color square = filter (not . isCheck color . move board) $ B.possibleMoves board color square 

rookValidMoves :: Board -> Color -> Square -> [Move]
rookValidMoves board color square = filter (not . isCheck color . move board) $ R.possibleMoves board color square 

queenValidMoves :: Board -> Color -> Square -> [Move]
queenValidMoves board color square = filter (not . isCheck color . move board) $ Q.possibleMoves board color square 

kingValidMoves :: Board -> Color -> Square -> [Move]
kingValidMoves board color square = filter (not . isCheck color . move board) $  K.possibleMoves board color square 

-- * нельзя ходить под шах
kingValidMoveSquares :: Color -> Square -> Board -> [Square]
kingValidMoveSquares color square board = filter (not . isCheck color . moveKing) $ K.moveSquares square
  where
    moveKing targetSquare = movePiece square targetSquare (Piece King color) board

kingCaptureThreatSquares :: Color -> Square -> Board -> [Square]
kingCaptureThreatSquares color square board
  = mapMaybe (findOrStop color board . (:[])) (kingValidMoveSquares color square board)

-- Ничья

-- 

-- Пат?
isStalemate :: Board -> Bool 
isStalemate board = not (isCheck (nextMove board) board) && null (possibleMoves board) 

-- https://en.wikipedia.org/wiki/Draw_(chess)
-- https://ru.wikipedia.org/wiki/%D0%9D%D0%B8%D1%87%D1%8C%D1%8F_(%D1%88%D0%B0%D1%85%D0%BC%D0%B0%D1%82%D1%8B)
isDeadPosition :: Board -> Bool 
isDeadPosition board | white == [kingWhite] && black == [kingBlack] = True
                     | white == [bishopWhite, kingWhite] && black == [kingBlack] = True
                     | white == [kingWhite] && black == [bishopBlack, kingBlack] = True
                     | white == [knightWhite, kingWhite] && black == [kingBlack] = True
                     | white == [kingWhite] && black == [knightBlack, kingBlack] = True
                     | otherwise = False
  where
    white = map snd $ pieces White board
    black = map snd $ pieces Black board

isFiftyMove :: Board -> Bool
isFiftyMove Board{..} = halfmoveClock >= 99

isThreefoldRepetition :: Board -> Bool
isThreefoldRepetition board = any (\(f, c) -> c >= 3) $ Map.toList $ fens board 

-- Шах?
-- Обязательно нужно передавть цвет, для того чтобы проверять не появляется ли шах
-- после моего хода (в Board nextMove уже будет записан новый цвет)
isCheck :: Color -> Board -> Bool
isCheck color board = kingAt color board `elem` allCheckThreatSquares (opponent color) board

-- Мат?
isMate :: Board -> Bool
isMate board = isCheck (nextMove board) board && null (possibleMoves board)

isDraw :: Board -> Bool
isDraw board
  =  isStalemate board
  || isDeadPosition board
  || isFiftyMove board
  || isThreefoldRepetition board

isOver :: Board -> Maybe Result 
isOver board | isMate board                = Just $ if nextMove board == White then BlackWon else WhiteWon
             | isStalemate board           = Just $ Draw Stalemate
             | isDeadPosition board        = Just $ Draw DeadPosition
             | isFiftyMove board           = Just $ Draw FiftyMove
             | isThreefoldRepetition board = Just $ Draw ThreefoldRepetition
             | otherwise                   = Nothing
