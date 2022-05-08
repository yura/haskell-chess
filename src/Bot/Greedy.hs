module Bot.Greedy where

import Board
import Laws
import Bot.Random

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  let moves = possibleMoves board
  let maxMove = maximum moves
  random <- randomMove moves

  return $ case maxMove of
      Move {} -> move board random
      _       -> move board maxMove

