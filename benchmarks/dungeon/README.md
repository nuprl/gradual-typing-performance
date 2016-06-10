dungeon
===

Maze generator.
Originally by Vincent.

Issues:
- Removed the `open?` init-field of the `Door%` class so that both `Door%` and `Cell%` classes could
  go inside an array (couldn't define a useful supertype otherwise).
- Not using `math/array` because `Mutable-Array` cannot go across type boundaries.
- Fully-typed is slow if there's a racket/dict boundary

Trouble with random numbers:
- `utils.rkt` keeps a hard-coded list of random numbers,
  but the typed & untyped code use DIFFERENT NUMBERS of these numbers. Off by 6.
