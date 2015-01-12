Soft Contracts
===

Benchmarks taken from the soft-contracts [paper](http://arxiv.org/pdf/1307.6239.pdf)'s [repository](https://github.com/philnguyen/soft-contract).

There are 3 programs:
  - zombie.rkt
  - snake.rkt
  - tetris.rkt
Each declares a contracted and un-contracted version of a graphical game.

There are 3 executable benchmarks:
  - run-zombie.rkt
  - run-snake.rkt
  - run-tetris.rkt
They will run a game on its saved history (i.e. `zombie-hist-4.txt`) and print the time taken to `stdout`.
