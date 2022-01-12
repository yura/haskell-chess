module Display where

import Board (Square, cols, rows)

-- |Имена полей в порядке, который удобен для вывода доски на экран
squaresDisplayOrder :: [[Square]]
squaresDisplayOrder = map colName $ reverse rows
  where
    colName row = map (\c -> (c, row)) cols

