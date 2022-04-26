module Bot.Random where

import Control.Monad.Random
import Board
import Laws

--randomMove :: (MonadRandom m) => [Move] -> m Move
randomMove :: [Move] -> IO Move
randomMove moves = do
  randomIndex <- getRandomR (0, length moves - 1)
  print moves
  print (moves !! randomIndex)

  return (moves !! randomIndex)

--makeMove :: (MonadRandom m) => Color -> Board -> [Move] -> m (Board, [Move])
makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  move board <$> randomMove (possibleMoves board)
