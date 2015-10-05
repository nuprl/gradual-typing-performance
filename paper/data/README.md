data
====

Experimental results.
Data for each project is stored in a `.rktd` file.


Data Format
-----------

These `.rktd` files each contain a single 2-dimensional vector.
The outer vector has `2**N` entries, one for each typed/untyped variation in the project's lattice.
The inner vector at position `i` is the absolute results of running the `i`-th variation.

We order variations by:
- Ordering the modules in the project alphabetically
  (Just the modules that can be typed or untyped, not the base files common to all variations.)
- Generating a bitstring for each variation, where the `k`-th bit is high iff the `k`-th module
  in the alphabetical ordering is typed. (Zero is the left end of the bitstring.)
- Mapping each bitstring to the natural number it represents in binary.

Example: the fourth entry in the `.rktd` data representing a project with four modules
would correspond to the variation `0100`.


Machines
--------
- Galicia (4 phys. cores, Core i7-3770K 3.50GHz)
- Cluster (40 phys. cores, Xeon CPU E5-2680 2.8GHz)
- Desktop (4 phys. cores, Core i7-4790 3.60GHz)
- Lambda (12 phys. cores, Xeon E5-2630 2.30GHz)


Files
-----

All data run on Racket 6.2.

- `gregor-2015-07-26T05:27:18.rktd` Run on Lambda, single-core.
- `kcfa-2015-06-25T13:48:52.rktd` Run on Lambda with `-j 11`
- `lnm-large-06-28.rktd` Run on Desktop, single core. Benchmarked on data for gregor.
- `mbta-04-25.rktd` Run on Galicia, single core.
- `morsecode-large-06-27.rktd` Run on Desktop, single core.
- `quad-galicia-07-26.rktd` Run on Galicia, 3 cores.
- `sieve-2015-06-28T14:08:55.rktd` Run on Lambda, single core
- `snake-2015-06-30T14:55:40.rktd` Run on Cluster, `-j 39`
- `suffixtree-large-06-30.rktd` Run on Desktop, single core.
- `synth-2015-07-02T01:47:43.rktd` Run on Cluster, `-j 19`
- `tetris-2015-07-01T16:39:46.rktd` Run on Cluster, `-j 19`
- `zordoz-04-09` Run on Galicia, single core.
