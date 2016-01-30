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
So the fourth entry in the `.rktd` data representing a project with four modules
would correspond to the variation `0100`.

There is no metadata stored in these data files.
We track that here.


Machines
--------
- Galicia (4 phys. cores, Core i7-3770K 3.50GHz)
- "The cluster" (40 phys. cores, Xeon CPU E5-2680 2.8GHz)
- Desktop (4 phys. cores, Core i7-4790 3.60GHz)
- Lambda (12 phys. cores, Xeon E5-2630 2.30GHz)
- Sapporo (2 phys. cores, PPC970MP)


Files
-----

Run on Racket 6.2.

- `gregor-2015-06-30.rktd` Run on cluster, `-j 39`
- `gregor-2015-07-02.rktd` Run on cluster, `-j 19`
- `gregor-2015-07-26T05:27:18.rktd` Run on lambda single-core.
- `kcfa-2015-06-25T13:48:52.rktd` Run on Lambda with `-j 11`
- `lnm-large-06-28.rktd` Run on Desktop, single core. Benchmarked on data for gregor.
- `morsecode-06-27.rktd` Run on Desktop, single core. Benchmarked on the entire list of frequently used words.
- `mbta-2015-09-07T23:17:52.rktd` Run on Desktop, single core. Single-threaded mbta.
- `sieve-2015-06-28T14:08:55.rktd` Run on Lambda, single core
- `suffixtree-large-06-30.rktd` Run on Desktop, single core, 30 iters.
- `synth-2015-06-30T15:34:39.rktd` Run on cluster, `-j 39`
- `synth-2015-07-02T01:47:43.rktd` Run on cluster, `-j 19`
- `snake-2015-06-30T14:55:40.rktd` Run on cluster, `-j 39`
- `snake-2015-07-01T16:35:34.rktd` Run on cluster, `-j 19`
- `tetris-2015-06-30T14:53:37.rktd` Run on cluster, `-j 39`
- `tetris-2015-07-01T16:39:46.rktd` Run on cluster, `-j 19`

###### Run on different-sized inputs
- `synth-mid-06-26.rktd` Run on Galicia, single core. Benchmarked on medium-size synth test (smoke on the water).
- `kcfa-small-06-27.rktd` Run on Desktop, single core. Benchmarked on a very small "standard example" test.
- `lnm-mid-06-22.rktd` Run on Desktop, single core. Benchmarked on data for suffixtree.
- `morsecode-small-06-27.rktd` Run on Desktop, single core. Benchmarked on a subset of the "frequently-used-words" file.

__NOTE__ all these files are _unofficial_, they were not run on Racket 6.2.
We need to update them all.

- zordoz.6.2.900.15-2015-09-09T20:10:57.rktd, For Racket v6.2.900.11, on Desktop
- zordoz-2015-09-09.rktd, A re-run of zordoz, on Desktop with Racket 6.2

- `echo.rktd` Unknown origin.
- `synth.rktd`
- `gregor-05-11.rktd` Run on Galicia
- `gregor-05-24.rktd` Run on the cluster.
- `kcfa-06-01.rktd` Run on Galicia.
- `mbta-04-20.rktd` Original MBTA, run on Galicia.
- `mbta-04-25.rktd` Fixed MBTA, run on Galicia.
- `mbta-singlethread-2015-09-07.rktd` Synchronous MBTA, run on Desktop, 6.2.900.15
- `sieve-04-06.rktd` Run on Galicia
- `snake-04-10.rktd` Run on Galicia
- `suffixtree-06-10.rktd` Run on Galicia
- `tetris.rktd` Run on Galicia, date unknown
- `zordoz-04-09.rktd` Run on Galicia

Without Any->Bool contracts
---------------------------
(Not sure I trust these, the graphs are funny and unexpected.
 Want to re-run both before & after on the same computer.)

- `tetris-anybool-09-11.rktd` on Desktop, 11 iterations, single core, 6.2.900.15
- `suffixtree-anybool-09-11.rktd` on Desktop, 11 iterations, single core, 6.2.900.15
