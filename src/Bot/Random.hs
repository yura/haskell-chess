module Bot.Random where

import Control.Monad.Random
import Board
import Laws

--randomMove :: (MonadRandom m) => [Move] -> m Move
randomMove :: [Move] -> IO Move
randomMove moves = do
  randomIndex <- getRandomR (0, length moves - 1)
  putStrLn $ show moves
  putStrLn $ show $ length moves
  putStrLn $ show randomIndex

  return (moves !! randomIndex)

--makeMove :: (MonadRandom m) => Color -> Board -> [Move] -> m (Board, [Move])
makeMove :: Color -> Board -> [Move] -> IO (Board, [Move])
makeMove color board moves = do
  let moves = possibleMoves color board
  nextMove <- randomMove moves
  return (move board nextMove, nextMove:moves)

