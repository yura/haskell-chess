# haskell-chess

## TODO

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

