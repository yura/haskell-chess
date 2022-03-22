module Main where

import           Control.Concurrent (threadDelay)
import Data.Text as T
import Text.Parsec

import Board
import Laws
import Board.InitialPosition
import Display
import qualified Format.PGN.Import as PGN
import qualified Format.PGN.Export as PGN
import Format.FEN ( toFEN )
import Format.FEN.Import ( fromFEN )

import Bot.Random
import Bot.Greedy
import Bot.Minimax

play
  :: (Board -> IO Board)
  -> (Board -> IO Board)
  ->  Board -> IO (Board, Result)
play bot1 bot2 board@Board{..} = do
  let bot = if nextMove == White then bot1 else bot2
  newBoard <- bot board
  
  renderBoard newBoard
  putStrLn $ toFEN newBoard

  threadDelay 125000

  let mResult = isOver (opponent nextMove) newBoard

  case mResult of
    Just result  -> return (newBoard, result)
    Nothing -> play bot1 bot2 newBoard

perfTest :: IO ()
perfTest = do
  let whiteLadderMatesIn3 = fromFEN "8/K7/5k2/8/8/8/3R4/4R3 w - - 0 1"
  let result = maxValue whiteLadderMatesIn3 5
  putStrLn $ show result
  
main :: IO ()
main = do
  -- perfTest
  let moves = parse PGN.parseGame "PGN" "1. e4 c6 2. d4 d5 3. Nc3 dxe4 4. Nxe4 Nf6 5. Nxf6+ exf6 6. Bc4 Be7 7. Qh5 O-O 8. Be3 Nd7 9. Nf3 Nb6 10. Bb3 Be6 11. c3 Bxb3 12. axb3 Nd5 13. O-O Nxe3 14. fxe3 Bd6 15. e4 Bc7 16. e5 fxe5 17. Nxe5 Bxe5 18. Qxe5 Re8 19. Qc5 Qb6 20. Qxb6 axb6 21. Kf2 Ra6 22. Rae1 Rxe1 23. Rxe1 Kf8 24. c4 Ra2 25. Re2 Ra3 26. Re3 f6 27. d5 Ra8 28. b4 Rd8 29. Rd3 Ke7 30. Ke3 Rd6 31. Ke4 g6 32. g4 Kd7 33. dxc6+ bxc6 34. Rxd6+ Kxd6 35. Kd4 c5+ 36. Ke4 cxb4 37. Kd4 f5 38. gxf5 gxf5 39. b3 f4 40. Ke4 Kc5 41. Kxf4 Kd4 42. h4 Kc3 43. Ke3 Kxb3 44. Kd2 Kxc4 45. Kc2 b3+ 46. Kb2 h5 47. Kb1 Kd4 48. Kb2 Ke4 49. Kxb3 Kf4 50. Kb4 Kg4 51. Kb5 Kxh4 52. Kxb6 Kg3 53. Kc5 h4 54. Kd4 h3 55. Kd3 h2 56. Ke4 h1=Q+ 57. Ke5 Qf3 58. Ke6 Kg4 59. Kd6 Qf4+ 60. Kc6 Kg5 61. Kc5 Qf6 62. Kd5 Kf4 63. Kc4 Qg5 64. Kb3 Qg4 0-1"

  let p1 = Bot.Minimax.makeMove
  --let p2 = Bot.Greedy.makeMove
  let p2 = Bot.Random.makeMove

  (board, result) <- play p1 p2 initialBoard
  renderBoard board

  print $ Prelude.reverse $ history board
  putStrLn $ PGN.exportMovesToPGN $ Prelude.reverse $ history board

  print result
