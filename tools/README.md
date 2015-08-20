tools
=====

Scripts for running our benchmarks.

- `benchmark-util/` Contains a Racket package which provides a `require/typed/check` form.
  This macro checks whether the required module is typed, and if so does not attach
   contracts to imported values.

- `data-lattice.rkt` Script for drawing data lattices.

- `run.rkt` Script for running all configurations in a lattice.

- `setup-benchmark.rkt` Script for synthesizing all lattice configurations from
  a fully-typed and fully-untyped versions of a benchmark.
