dungeon
===

Maze generator.
Originally by Vincent.

Issues:
- Removed the `open?` init-field of the `Door%` class so that both `Door%` and `Cell%` classes could
  go inside an array (couldn't define a useful supertype otherwise).
- Not using `math/array` because `Mutable-Array` cannot go across type boundaries.
- Fully-typed is slow if there's a racket/dict boundary
