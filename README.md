# haskell-chess

## TODO

* Неоптимальныые ходы: 
   * 33, 35, 36, 37 - не рубит пешку
   * 49 (b3),  50 (b3 или Qb4), 51 (Qa6 или Qb4) - не находит мат в 1 ход

1. Nb1a3 d7d6 2. Na3b5 Bc8g4 3. Ra1b1 Bg4xe2 4. Ke1xe2 a7a6 5. Nb5d4 Ng8h6 6. Ke2e3 a6a5 7. Qd1h5 c7c5 8. Bf1b5 Nb8c6 9. Bb5xc6 b7xc6 10. Nd4xc6 e7e5 11. Nc6xd8 Ke8xd8 12. Qh5h4 f7f6 13. Ke3e4 Nh6f5 14. Ke4xf5 Ra8a6 15. Kf5e6 Kd8c7 16. Ke6f7 Kc7c6 17. Kf7e6 Bf8e7 18. Ke6xe7 Rh8e8 19. Ke7xe8 Ra6a7 20. Ke8f8 h7h5 21. Qh4xh5 Kc6b7 22. Qh5f7 Kb7c6 23. Qf7xa7 g7g5 24. Qa7xa5 Kc6d7 25. Kf8g8 Kd7e8 26. Kg8h8 Ke8d7 27. Kh8h7 f6f5 28. Kh7h8 e5e4 29. Kh8h7 Kd7c6 30. Kh7h8 c5c4 31. Qa5xf5 Kc6b6 32. Qf5xe4 Kb6a5 33. Kh8h7 g5g4 34. Qe4xg4 Ka5b6 35. Kh7h8 Kb6a5 36. Kh8h7 Ka5a4 37. Kh7h8 d6d5 38. Kh8h7 Ka4b4 39. Kh7h8 Kb4a4 40. Kh8h7 Ka4a5 41. Kh7h6 Ka5a6 42. Kh6h7 Ka6a5 43. Kh7h6 Ka5b5 44. Kh6h5 Kb5a4 45. Kh5h6 d5d4 46. Qg4xd4 Ka4b4 47. Kh6h7 Kb4a4 48. Qd4b6 c4c3 49. a2a3 c3xd2 50. Kh7h8 d2xc1=Q 51. Qb6c6 Ka4a5 52. b2b4


1. Na3 { A00 Sodium Attack } g5 2. Nb5 c5 3. Nxa7 Rxa7 4. Rb1 Rxa2 5. Ra1 Rxb2 6. Bxb2 c4 7. Bxh8 Qb6 8. Ra8 Qxf2+ 9. Kxf2 b6 10. Rxb8 h6 11. Rxc8# { White wins by checkmate. } 1-0

* isDraw :: Color -> Board -> Bool https://en.wikipedia.org/wiki/Draw_(chess)
  * king and bishop versus king and bishop with the bishops on the same color
* Random bot: make random moves but following all chess rules
* Lichess integration -- получить рейтинг Эло
* Read PGN: -- нужны все возможные ходы, как их правильно записывать
  * прогнать parseGame на всей базе
  * draw =
  * ignore comments {} or ;

* Brute-force bot: make best move looking N moves ahead

* Openings Book https://www.chessprogramming.org/Opening_Book
  * Polygon
    * http://hgm.nubati.net/book_format.html
    * https://python-chess.readthedocs.io/en/latest/pgn.html
    * https://github.com/sshivaji/polyglot
    * https://github.com/mlang/chessIO
  * https://github.com/lichess-org/chess-openings
  * https://www.pgnmentor.com/files.html#openings
  * Descriptions https://en.wikibooks.org/wiki/Chess_Opening_Theory
  * ECO https://www.chessprogramming.org/ECO http://www.chessarch.com/library/0000_eco/index.shtml
  * Alternative format https://www.chessprogramming.org/NIC-Key

## FIDE Laws of Chess

* https://www.fide.com/FIDE/handbook/LawsOfChess.pdf

## All possible moves

```sh
ruby get_uniq_moves.rb lichess_db_standard_rated_2013-01.pgn > uniq_moves.txt
```

## Other libs

### BishBosh https://github.com/functionalley/BishBosh

A chess-game which can be rendered in a terminal using raw ASCII, or used as an engine by xboard

### Chesshs https://hackage.haskell.org/package/chesshs

With this library you can load chess boards from FEN and PGN notation and apply moves to the boards. Moves will only be allowed if they are valid under the normal chess rules.

### chessIO https://github.com/mlang/chessIO

A simple and fast library for generating legal chess moves. Also includes a module for communication with external processes that speak the UCI (Universal Chess Interface) protocol, a PGN parser/pretty printer, and Polyglot opening book support. On top of that, provides a console frontend program (cboard) that can be used to interactively play against UCI engines.

### hchesslib https://github.com/nablaa/hchesslib

Library implementing chess rules.


### https://github.com/tromp/ChessPositionRanking


## DONE
### 21.02.2022
* Рокировка https://en.wikipedia.org/wiki/Castling

### 18.02.2022
* isCheck :: Color -> Board -> Bool
* isMate :: Color -> Board -> Bool
  * return $ (moves !! randomIndex) -- negative index или index too large
* Export to PGN

* Read PGN:
  + check +
  + checkmate #
  + position annotion https://en.wikipedia.org/wiki/Chess_annotation_symbols

