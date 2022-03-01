module Bot.Minimax where

import Data.List (find)
import Board
import Laws
import Bot.Random
import Board (opponent)

makeMove :: Board -> IO Board
makeMove board@Board{..} = do
  let moves = possibleMoves nextMove board
  let winningMove = findWinningMove board moves
  let twoStepWinningMove = findTwoStepWin board
  let betterMoves = eliminateLosingMoves board
  
  random       <- randomMove moves

  case winningMove of
      Just m ->  return $ move board m
      _      -> case twoStepWinningMove of
        Just m2 -> return $ move board m2
        _       -> case betterMoves of
          [] -> return $ move board random
          _  -> do
            betterRandom <- randomMove betterMoves
            return $ move board betterRandom

findTwoStepWin :: Board -> Maybe Move
findTwoStepWin board@Board{..} = find (null . eliminateLosingMoves . move board) moves
  where
    moves = possibleMoves nextMove board
      
eliminateLosingMoves :: Board -> [Move]
eliminateLosingMoves board@Board{..}
  = filter (\m -> null . findWinningMove (newBoard m) 
  $ possibleMoves (opponent nextMove) (newBoard m)) moves
  where
    moves = possibleMoves nextMove board
    newBoard m = move board m 

findWinningMove :: Board -> [Move] -> Maybe Move
findWinningMove board@Board{..}  = find (isMate (opponent nextMove) . move board)

{-
def find_two_step_win(game_state, next_player):
  opponent = next_player.other()
  for candidate_move in game_state.legal_moves(next_player):        #1
    next_state = game_state.apply_move(candidate_move)              #2
    good_responses = eliminate_losing_moves(next_state, opponent)   #3
    if not good_responses:                                          #3
      return candidate_move                                         #3
  return None 

def eliminate_losing_moves(game_state, next_player):
  opponent = next_player.other()
  possible_moves = []
  for candidate_move in game_state.legal_moves(next_player):
    next_state = game_state.apply_move(candidate_move)
    opponent_winning_move = find_winning_move(next_state, opponent)
    if opponent_winning_move is None:
      possible_moves.append(candidate_move)
  return possible_moves
 
def find_winning_move(game_state, next_player):
  for candidate_move in game_state.legal_moves(next_player):
    next_state = game_state.apply_move(candidate_move)
    if next_state.is_over() and next_state.winner == next_player:
      return candidate_move
  return None
-}
