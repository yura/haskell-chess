module Main where

import           Control.Concurrent (threadDelay)
import Data.Text as T
import Text.Parsec

import Board
import Laws
import Board.InitialPosition
import Display
import qualified Format.PGN as PGN

import Bot.Random
import Board

play
  :: (Color -> Board -> [Move] -> IO (Board, [Move]))
  -> (Color -> Board -> [Move] -> IO (Board, [Move]))
  ->  Color -> Board -> [Move] -> IO (Board, [Move], Result)
play bot1 bot2 color board moves = do
  let bot = if color == White then bot1 else bot2
  (newBoard, newMoves) <- bot color board moves
  renderBoard newBoard
  print $ Prelude.head newMoves

  -- threadDelay 500000

  let mResult = result (opponent color) newBoard

  case mResult of
    Just r  -> return (newBoard, newMoves, r)
    Nothing -> play bot1 bot2 (opponent color) newBoard newMoves

main :: IO ()
main = do
  let moves = parse PGN.parseGame "PGN" "1. e4 c6 2. d4 d5 3. Nc3 dxe4 4. Nxe4 Nf6 5. Nxf6+ exf6 6. Bc4 Be7 7. Qh5 O-O 8. Be3 Nd7 9. Nf3 Nb6 10. Bb3 Be6 11. c3 Bxb3 12. axb3 Nd5 13. O-O Nxe3 14. fxe3 Bd6 15. e4 Bc7 16. e5 fxe5 17. Nxe5 Bxe5 18. Qxe5 Re8 19. Qc5 Qb6 20. Qxb6 axb6 21. Kf2 Ra6 22. Rae1 Rxe1 23. Rxe1 Kf8 24. c4 Ra2 25. Re2 Ra3 26. Re3 f6 27. d5 Ra8 28. b4 Rd8 29. Rd3 Ke7 30. Ke3 Rd6 31. Ke4 g6 32. g4 Kd7 33. dxc6+ bxc6 34. Rxd6+ Kxd6 35. Kd4 c5+ 36. Ke4 cxb4 37. Kd4 f5 38. gxf5 gxf5 39. b3 f4 40. Ke4 Kc5 41. Kxf4 Kd4 42. h4 Kc3 43. Ke3 Kxb3 44. Kd2 Kxc4 45. Kc2 b3+ 46. Kb2 h5 47. Kb1 Kd4 48. Kb2 Ke4 49. Kxb3 Kf4 50. Kb4 Kg4 51. Kb5 Kxh4 52. Kxb6 Kg3 53. Kc5 h4 54. Kd4 h3 55. Kd3 h2 56. Ke4 h1=Q+ 57. Ke5 Qf3 58. Ke6 Kg4 59. Kd6 Qf4+ 60. Kc6 Kg5 61. Kc5 Qf6 62. Kd5 Kf4 63. Kc4 Qg5 64. Kb3 Qg4 0-1"

  let p1 = Bot.Random.makeMove
  let p2 = Bot.Random.makeMove

  (board, moves, res) <- play p1 p2 White initialBoard []
  renderBoard board
  print res
  print $ Prelude.reverse moves
  -- print $ exportToPGN $ reverse moves

  return ()

-- Stalemate
-- [Move (Piece Pawn White) ('e',2) ('e',4),Move (Piece Pawn Black) ('f',7) ('f',5),Move (Piece Pawn White) ('g',2) ('g',3),Move (Piece Pawn Black) ('d',7) ('d',5),Move (Piece Bishop White) ('f',1) ('d',3),Move (Piece Pawn Black) ('e',7) ('e',5),Move (Piece Queen White) ('d',1) ('h',5),Move (Piece King Black) ('e',8) ('e',7),Capture (Piece Queen White) ('h',5) ('h',7),Move (Piece King Black) ('e',7) ('f',6),Move (Piece Bishop White) ('d',3) ('c',4),Move (Piece Pawn Black) ('b',7) ('b',5),Move (Piece Pawn White) ('b',2) ('b',4),Move (Piece Queen Black) ('d',8) ('e',8),Move (Piece Pawn White) ('d',2) ('d',3),Move (Piece Pawn Black) ('g',7) ('g',6),Move (Piece Bishop White) ('c',1) ('a',3),Move (Piece King Black) ('f',6) ('g',5),Move (Piece Pawn White) ('f',2) ('f',4),Move (Piece King Black) ('g',5) ('g',4),Capture (Piece Queen White) ('h',7) ('g',6),Capture (Piece Queen Black) ('e',8) ('g',6),Capture (Piece Pawn White) ('e',4) ('d',5),Move (Piece Bishop Black) ('f',8) ('d',6),Move (Piece King White) ('e',1) ('f',2),Move (Piece Queen Black) ('g',6) ('h',6),Move (Piece King White) ('f',2) ('g',2),Move (Piece Queen Black) ('h',6) ('f',8),Move (Piece Bishop White) ('c',4) ('b',3),Move (Piece Pawn Black) ('c',7) ('c',6),Move (Piece Knight White) ('g',1) ('h',3),Move (Piece Queen Black) ('f',8) ('g',7),Move (Piece Knight White) ('h',3) ('f',2),Move (Piece King Black) ('g',4) ('h',5),Capture (Piece Pawn White) ('d',5) ('c',6),Move (Piece King Black) ('h',5) ('h',6),Capture (Piece Pawn White) ('f',4) ('e',5),Move (Piece King Black) ('h',6) ('h',7),Move (Piece Pawn White) ('h',2) ('h',3),Move (Piece Queen Black) ('g',7) ('f',6),Move (Piece Rook White) ('h',1) ('d',1),Move (Piece Queen Black) ('f',6) ('h',4),Move (Piece Knight White) ('f',2) ('g',4),Capture (Piece Queen Black) ('h',4) ('g',4),Move (Piece Pawn White) ('d',3) ('d',4),Move (Piece Knight Black) ('g',8) ('f',6),Move (Piece Pawn White) ('c',2) ('c',3),Move (Piece Bishop Black) ('d',6) ('c',5),Move (Piece Bishop White) ('b',3) ('d',5),Move (Piece Bishop Black) ('c',8) ('d',7),Move (Piece Bishop White) ('d',5) ('g',8),Capture (Piece Queen Black) ('g',4) ('g',8),Move (Piece King White) ('g',2) ('h',1),Move (Piece King Black) ('h',7) ('g',7),Move (Piece King White) ('h',1) ('g',1),Move (Piece Knight Black) ('f',6) ('e',8),Move (Piece Rook White) ('d',1) ('f',1),Move (Piece Queen Black) ('g',8) ('e',6),Move (Piece Knight White) ('b',1) ('d',2),Move (Piece Queen Black) ('e',6) ('h',6),Move (Piece King White) ('g',1) ('f',2),Move (Piece Bishop Black) ('c',5) ('e',7),Move (Piece King White) ('f',2) ('e',1),Move (Piece Bishop Black) ('e',7) ('h',4),Move (Piece King White) ('e',1) ('d',1),Move (Piece King Black) ('g',7) ('h',7),Move (Piece Knight White) ('d',2) ('f',3),Capture (Piece Queen Black) ('h',6) ('c',6),Move (Piece Rook White) ('f',1) ('g',1),Move (Piece Bishop Black) ('d',7) ('c',8),Capture (Piece Knight White) ('f',3) ('h',4),Move (Piece Queen Black) ('c',6) ('c',4),Move (Piece King White) ('d',1) ('d',2),Move (Piece Queen Black) ('c',4) ('c',7),Move (Piece Rook White) ('a',1) ('c',1),Move (Piece Knight Black) ('b',8) ('d',7),Move (Piece Rook White) ('c',1) ('d',1),Move (Piece Knight Black) ('e',8) ('g',7),Move (Piece Knight White) ('h',4) ('g',6),Move (Piece King Black) ('h',7) ('h',6),Move (Piece Pawn White) ('g',3) ('g',4),Move (Piece Queen Black) ('c',7) ('d',6),Move (Piece Pawn White) ('d',4) ('d',5),Move (Piece Knight Black) ('g',7) ('h',5),Move (Piece Knight White) ('g',6) ('f',4),Capture (Piece Knight Black) ('d',7) ('e',5),Move (Piece Pawn White) ('h',3) ('h',4),Move (Piece Knight Black) ('e',5) ('g',6),Move (Piece Knight White) ('f',4) ('h',3),Move (Piece Knight Black) ('g',6) ('e',5),Move (Piece King White) ('d',2) ('e',3),Move (Piece Pawn Black) ('a',7) ('a',6),Capture (Piece Pawn White) ('g',4) ('f',5),Move (Piece Knight Black) ('h',5) ('f',6),Move (Piece Knight White) ('h',3) ('g',5),Move (Piece Rook Black) ('h',8) ('d',8),Move (Piece Rook White) ('g',1) ('h',1),Move (Piece Knight Black) ('e',5) ('c',4),Move (Piece King White) ('e',3) ('d',3),Move (Piece Queen Black) ('d',6) ('g',3),Move (Piece King White) ('d',3) ('c',2),Capture (Piece Queen Black) ('g',3) ('c',3),Capture (Piece King White) ('c',2) ('c',3),Move (Piece Knight Black) ('c',4) ('a',5),Move (Piece Rook White) ('d',1) ('d',4),Capture (Piece Knight Black) ('f',6) ('d',5),Capture (Piece Rook White) ('d',4) ('d',5),Move (Piece Bishop Black) ('c',8) ('b',7),Capture (Piece Rook White) ('d',5) ('d',8),Move (Piece Bishop Black) ('b',7) ('c',8),Move (Piece Rook White) ('h',1) ('c',1),Move (Piece King Black) ('h',6) ('g',7),Move (Piece Rook White) ('d',8) ('d',4),Move (Piece Bishop Black) ('c',8) ('d',7),Move (Piece King White) ('c',3) ('b',2),Move (Piece Bishop Black) ('d',7) ('e',6),Capture (Piece Knight White) ('g',5) ('e',6),Move (Piece King Black) ('g',7) ('h',6),Move (Piece Rook White) ('c',1) ('c',7),Move (Piece Knight Black) ('a',5) ('b',7),Move (Piece Rook White) ('c',7) ('c',5),Move (Piece Rook Black) ('a',8) ('f',8),Move (Piece Rook White) ('c',5) ('c',8),Move (Piece Knight Black) ('b',7) ('d',8),Move (Piece Pawn White) ('f',5) ('f',6),Capture (Piece Knight Black) ('d',8) ('e',6),Move (Piece Rook White) ('d',4) ('d',5),Move (Piece King Black) ('h',6) ('h',7),Move (Piece King White) ('b',2) ('b',1),Move (Piece Pawn Black) ('a',6) ('a',5),Move (Piece King White) ('b',1) ('a',1),Capture (Piece Pawn Black) ('a',5) ('b',4),Capture (Piece Rook White) ('c',8) ('f',8),Move (Piece Knight Black) ('e',6) ('c',7),Move (Piece Rook White) ('d',5) ('d',1),Move (Piece Knight Black) ('c',7) ('a',6),Move (Piece Rook White) ('f',8) ('g',8),Move (Piece Knight Black) ('a',6) ('b',8),Move (Piece Pawn White) ('h',4) ('h',5),Capture (Piece King Black) ('h',7) ('g',8),Move (Piece King White) ('a',1) ('b',1),Move (Piece King Black) ('g',8) ('h',8),Move (Piece Rook White) ('d',1) ('d',3),Move (Piece Knight Black) ('b',8) ('d',7),Move (Piece Bishop White) ('a',3) ('c',1),Move (Piece King Black) ('h',8) ('g',8),Move (Piece Rook White) ('d',3) ('g',3),Move (Piece King Black) ('g',8) ('f',7),Move (Piece Bishop White) ('c',1) ('d',2),Move (Piece King Black) ('f',7) ('e',6),Move (Piece Pawn White) ('a',2) ('a',3),Move (Piece King Black) ('e',6) ('d',6),Move (Piece Rook White) ('g',3) ('b',3),Move (Piece Knight Black) ('d',7) ('e',5),Move (Piece King White) ('b',1) ('b',2),Move (Piece King Black) ('d',6) ('c',6),Move (Piece Bishop White) ('d',2) ('e',1),Move (Piece King Black) ('c',6) ('b',7),Move (Piece Rook White) ('b',3) ('c',3),Capture (Piece Pawn Black) ('b',4) ('a',3),Move (Piece King White) ('b',2) ('b',1),Move (Piece Knight Black) ('e',5) ('g',4),Move (Piece King White) ('b',1) ('a',1),Move (Piece King Black) ('b',7) ('b',8),Move (Piece Pawn White) ('f',6) ('f',7),Move (Piece Knight Black) ('g',4) ('f',2),Capture (Piece Bishop White) ('e',1) ('f',2),Move (Piece King Black) ('b',8) ('a',8),Move (Piece Rook White) ('c',3) ('c',6),Move (Piece Pawn Black) ('b',5) ('b',4),Move (Piece Bishop White) ('f',2) ('d',4),Move (Piece King Black) ('a',8) ('b',8),Move (Piece Rook White) ('c',6) ('b',6),Move (Piece King Black) ('b',8) ('c',8),Move (Piece Rook White) ('b',6) ('d',6),Move (Piece King Black) ('c',8) ('c',7),Move (Piece Rook White) ('d',6) ('a',6),Move (Piece King Black) ('c',7) ('d',8),Move (Piece Bishop White) ('d',4) ('c',5),Move (Piece Pawn Black) ('a',3) ('a',2),Capture (Piece Bishop White) ('c',5) ('b',4),Move (Piece King Black) ('d',8) ('c',8),Promotion ('f',7) ('f',8) (Piece Queen White),Move (Piece King Black) ('c',8) ('b',7),Move (Piece Rook White) ('a',6) ('a',4),Move (Piece King Black) ('b',7) ('c',6),Move (Piece Rook White) ('a',4) ('a',8),Move (Piece King Black) ('c',6) ('b',7),Move (Piece Queen White) ('f',8) ('d',8),Move (Piece King Black) ('b',7) ('c',6),Capture (Piece King White) ('a',1) ('a',2),Move (Piece King Black) ('c',6) ('b',7),Move (Piece Queen White) ('d',8) ('d',4),Move (Piece King Black) ('b',7) ('c',7),Move (Piece Queen White) ('d',4) ('f',6),Move (Piece King Black) ('c',7) ('b',7),Move (Piece Queen White) ('f',6) ('d',6),Capture (Piece King Black) ('b',7) ('a',8),Move (Piece Queen White) ('d',6) ('d',8),Move (Piece King Black) ('a',8) ('a',7),Move (Piece Pawn White) ('h',5) ('h',6),Move (Piece King Black) ('a',7) ('b',7),Move (Piece Bishop White) ('b',4) ('e',7),Move (Piece King Black) ('b',7) ('a',7),Move (Piece King White) ('a',2) ('b',3),Move (Piece King Black) ('a',7) ('a',6),Move (Piece Queen White) ('d',8) ('d',2),Move (Piece King Black) ('a',6) ('a',7),Move (Piece Queen White) ('d',2) ('d',7),Move (Piece King Black) ('a',7) ('a',8),Move (Piece Queen White) ('d',7) ('d',5),Move (Piece King Black) ('a',8) ('a',7),Move (Piece Bishop White) ('e',7) ('f',6),Move (Piece King Black) ('a',7) ('b',6),Move (Piece Pawn White) ('h',6) ('h',7),Move (Piece King Black) ('b',6) ('a',6),Move (Piece Bishop White) ('f',6) ('a',1),Move (Piece King Black) ('a',6) ('a',7),Move (Piece Queen White) ('d',5) ('f',5),Move (Piece King Black) ('a',7) ('a',8),Move (Piece Queen White) ('f',5) ('d',5),Move (Piece King Black) ('a',8) ('b',8),Move (Piece Queen White) ('d',5) ('c',6),Move (Piece King Black) ('b',8) ('a',7),Move (Piece King White) ('b',3) ('c',3),Move (Piece King Black) ('a',7) ('b',8),Move (Piece Queen White) ('c',6) ('c',4),Move (Piece King Black) ('b',8) ('a',7),Promotion ('h',7) ('h',8) (Piece Rook White),Move (Piece King Black) ('a',7) ('b',6),Move (Piece King White) ('c',3) ('b',4),Move (Piece King Black) ('b',6) ('b',7),Move (Piece Rook White) ('h',8) ('d',8),Move (Piece King Black) ('b',7) ('a',7),Move (Piece Queen White) ('c',4) ('c',7),Move (Piece King Black) ('a',7) ('a',6),Move (Piece Rook White) ('d',8) ('d',7)]

-- Мат
-- [Move (Piece Pawn White) ('b',2) ('b',4),Move (Piece Pawn Black) ('d',7) ('d',6),Move (Piece Pawn White) ('e',2) ('e',4),Move (Piece Bishop Black) ('c',8) ('d',7),Move (Piece Bishop White) ('f',1) ('d',3),Move (Piece Pawn Black) ('b',7) ('b',6),Move (Piece Bishop White) ('d',3) ('a',6),Move (Piece Knight Black) ('g',8) ('h',6),Move (Piece Knight White) ('g',1) ('e',2),Move (Piece Rook Black) ('h',8) ('g',8),Move (Piece Bishop White) ('a',6) ('c',8),Move (Piece Pawn Black) ('e',7) ('e',5),Move (Piece Rook White) ('h',1) ('g',1),Move (Piece Queen Black) ('d',8) ('h',4),Move (Piece Pawn White) ('a',2) ('a',3),Move (Piece Knight Black) ('b',8) ('a',6),Move (Piece King White) ('e',1) ('f',1),Move (Piece Bishop Black) ('d',7) ('g',4),Move (Piece Bishop White) ('c',1) ('b',2),Move (Piece Pawn Black) ('g',7) ('g',6),Move (Piece Rook White) ('g',1) ('h',1),Capture (Piece Queen Black) ('h',4) ('h',2),Move (Piece Bishop White) ('c',8) ('f',5),Move (Piece Pawn Black) ('g',6) ('g',5),Move (Piece Queen White) ('d',1) ('e',1),Move (Piece King Black) ('e',8) ('d',8),Capture (Piece Bishop White) ('b',2) ('e',5),Move (Piece Pawn Black) ('c',7) ('c',5),Move (Piece Knight White) ('e',2) ('f',4),Move (Piece Pawn Black) ('d',6) ('d',5),Move (Piece Pawn White) ('a',3) ('a',4),Move (Piece Queen Black) ('h',2) ('h',3),Move (Piece Bishop White) ('e',5) ('b',2),Move (Piece Rook Black) ('a',8) ('b',8),Move (Piece Queen White) ('e',1) ('e',3),Move (Piece Rook Black) ('b',8) ('c',8),Move (Piece Bishop White) ('f',5) ('e',6),Capture (Piece Queen Black) ('h',3) ('h',1)]