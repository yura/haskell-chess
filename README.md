# haskell-chess

## TODO

* Random bot: make random moves but following all chess rules
* Brute-force bot: make best move looking N moves ahead
* Plug into the Lichess platform to get ELO rating
* Read PGN
* Read FEN

## FIDE Laws of Chess

* https://www.fide.com/FIDE/handbook/LawsOfChess.pdf

## All possible moves

```sh
ruby get_uniq_moves.rb lichess_db_standard_rated_2013-01.pgn > uniq_moves.txt
```
