# haskell-chess

## TODO

* Read PGN: -- нужны все возможные ходы, как их правильно записывать
  * draw =
  * result: 1-0, 0-1, 1/2-1/2
  * position annotion https://en.wikipedia.org/wiki/Chess_annotation_symbols
  * ignore comments {} or ;
* Random bot: make random moves but following all chess rules
* Lichess integration -- получить рейтинг Эло

* Brute-force bot: make best move looking N moves ahead
* Read FEN -- нужно для решения задач
* Write PGN

## FIDE Laws of Chess

* https://www.fide.com/FIDE/handbook/LawsOfChess.pdf

## All possible moves

```sh
ruby get_uniq_moves.rb lichess_db_standard_rated_2013-01.pgn > uniq_moves.txt
```

## DONE

* Read PGN:
  + check +
  + checkmate #

