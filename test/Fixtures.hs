module Fixtures where

import Board (Board)
import Format.FEN.Import ( fromFEN )

-- Линейный мат в два хода
-- https://lichess.org/editor/7k/K7/8/8/8/8/5R2/4R3_w_-_-_0_1
ladderMateIn2 :: Board
ladderMateIn2 = fromFEN "7k/K7/8/8/8/8/5R2/4R3 w - - 0 1"

-- Линейный мат в три хода
-- https://lichess.org/editor/8/K7/5k2/8/8/8/3R4/4R3_w_-_-_0_1
ladderMateIn3 :: Board
ladderMateIn3 = fromFEN "8/K7/5k2/8/8/8/3R4/4R3 w - - 0 1"

-- Позиции с матом чёрным
blackMated :: [Board]
blackMated =
  [ blackMatedByQueen
  , scholarsMate
  ]

-- Позиции с матом белым
whiteMated :: [Board]
whiteMated =
  [ whiteMatedByRookAndKing
  , foolsMate
  ]

-- Мат в углу ферзём под прикрытием короля
-- https://lichess.org/editor/7k/5KQ1/8/8/8/8/8/8_b_-_-_0_1
blackMatedByQueen :: Board
blackMatedByQueen = fromFEN "7k/5KQ1/8/8/8/8/8/8 b - - 0 1"

-- Один ход до мата
-- https://lichess.org/editor/7k/5K2/8/8/8/6Q1/8/8_w_-_-_0_1
blackMatedByQueenIn1 :: Board
blackMatedByQueenIn1 = fromFEN "7k/5K2/8/8/8/6Q1/8/8 w - - 0 1"

-- Мат ладьёй и королём
-- https://lichess.org/editor/Q7/8/8/8/8/3k4/8/3K2r1_w_-_-_0_1
whiteMatedByRookAndKing :: Board 
whiteMatedByRookAndKing = fromFEN "Q7/8/8/8/8/3k4/8/3K2r1 w - - 0 1"

-- Линейный мат двумя ладьями

-- Десткий мат
-- https://lichess.org/editor/r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR_b_KQkq_-_0_1
scholarsMate :: Board
scholarsMate = fromFEN "r1bqkb1r/pppp1Qpp/2n2n2/4p3/2B1P3/8/PPPP1PPP/RNB1K1NR b KQkq - 0 1"

-- Дурацкий мат
-- https://lichess.org/editor/rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR_w_KQkq_-_0_1
foolsMate :: Board
foolsMate = fromFEN "rnb1kbnr/pppp1ppp/8/4p3/6Pq/5P2/PPPPP2P/RNBQKBNR w KQkq - 0 1"

-- Можно избежать мата, срубив шахующую ладью
-- https://lichess.org/editor/Q7/8/8/8/8/3k4/8/3K3r_w_-_-_0_1
whiteCanAvoidMate :: Board
whiteCanAvoidMate = fromFEN "Q7/8/8/8/8/3k4/8/3K3r w - - 0 1"
