module Laws.Bishop where

import           Data.Maybe (fromJust, isJust)
import           Board
import           Laws.Util

sw :: Square -> [Square]
sw (col, row) = zip [pred col, pred (pred col)..head cols] [pred row, pred (pred row)..head rows]

se :: Square -> [Square]
se (col, row) = zip [succ col..last cols] [pred row, pred (pred row)..head rows]

ne :: Square -> [Square]
ne (col, row) = zip [succ col.. last cols] [succ row..last rows]

nw :: Square -> [Square]
nw (col, row) = zip [pred col, pred (pred col)..head cols] [succ row..last rows]

bishopMovesGrouped :: Square -> [[Square]]
bishopMovesGrouped square = filter (not . null) $ map (\f -> f square) [sw, se, ne, nw]

bishopMoves :: Square -> [Square]
bishopMoves = concat . bishopMovesGrouped

captureThreatSquares :: Color -> Square -> Board -> [Square]
captureThreatSquares color square board
  = map fromJust
  $ filter isJust
  $ map (findOrStop color board)
  $ bishopMovesGrouped square
