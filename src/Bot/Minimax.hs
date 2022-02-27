module Bot.Minimax where

import Data.List (find)
import Board
import Laws
import Bot.Random

makeMove :: Color -> Board -> [Move] -> IO (Board, [Move])
makeMove color board history = do
  let moves = possibleMoves color board
  let winningMove = findWinningMove color board moves
  let betterMoves = eliminateLosingMoves color board moves
  
  random       <- randomMove moves

  case winningMove of
      Just m ->  return (move board m, m : history)
      _      -> case betterMoves of
        [] -> return (move board random, random : history)
        _  -> do
          betterRandom <- randomMove betterMoves
          return (move board betterRandom, betterRandom : history)

findWinningMove :: Color -> Board -> [Move] -> Maybe Move
findWinningMove color board  = find (isMate (opponent color) . move board)

eliminateLosingMoves :: Color -> Board -> [Move] -> [Move]
eliminateLosingMoves color board
  = filter (\m -> null . findWinningMove (opponent color) (newBoard m) $ possibleMoves (opponent color) (newBoard m))
  where
    newBoard m = move board m 

{-
def eliminate_losing_moves(game_state, next_player):
  opponent = next_player.other()
  possible_moves = []
  for candidate_move in game_state.legal_moves(next_player):
    next_state = game_state.apply_move(candidate_move)
    opponent_winning_move = find_winning_move(next_state, opponent)
    if opponent_winning_move is None:
      possible_moves.append(candidate_move)
  return possible_moves
-}

{-  
def find_winning_move(game_state, next_player):
  for candidate_move in game_state.legal_moves(next_player):
    next_state = game_state.apply_move(candidate_move)
    if next_state.is_over() and next_state.winner == next_player:
      return candidate_move
  return None
-}
