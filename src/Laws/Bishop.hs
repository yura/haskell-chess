module Laws.Bishop where

import           Data.Maybe (mapMaybe)
import           Board
import           Laws.Util

sw :: Square -> [Square]
sw (Square s)
  = takeWhile (\square -> squareFile square < lastFile  && squareRank square >= firstRank)
  $ map nextSquare [9, 18..]
  where
    nextSquare :: Int -> Square
    nextSquare i = Square $ s - i

se :: Square -> [Square]
se (Square s)
  = takeWhile (\square -> squareFile square /= firstFile && squareRank square >= firstRank)
  $ map nextSquare [7, 14..]
  where
    nextSquare i = Square $ s - i

ne :: Square -> [Square]
ne (Square s)
  = takeWhile (\square -> squareFile square /= firstFile && squareRank square <= lastRank)
  $ map nextSquare [9, 18..]
  where
    nextSquare i = Square $ s + i

nw :: Square -> [Square]
nw (Square s)
  = takeWhile (\square -> squareFile square < lastFile && squareRank square <= lastRank)
  $ map nextSquare [7, 14..]
  where
    nextSquare i = Square $ s + i

bishopMovesGrouped :: Square -> [[Square]]
bishopMovesGrouped square = filter (not . null) $ map (\f -> f square) [sw, se, ne, nw]

bishopMoves :: Square -> [Square]
bishopMoves = concat . bishopMovesGrouped

underAttackSquares :: Board -> Color -> Square -> [Square]
underAttackSquares board color square = concatMap (filterAllEmptyOrFirstOpposite board color) $ bishopMovesGrouped square

moveSquares :: Color -> Square -> Board -> [Square]
moveSquares color square board =  filter (`notElem` captureThreatSquares color square board) $ underAttackSquares board color square 

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = mapMaybe (findOrStop color board) (bishopMovesGrouped square)

possibleMoves :: Board -> Color -> Square -> [Move]
possibleMoves board color square = captures ++ moves
  where
    piece = Piece Bishop color
    captures = map (Capture piece square) $ captureThreatSquares color square board
    moves    = map (Move piece square)    $ moveSquares color square board
