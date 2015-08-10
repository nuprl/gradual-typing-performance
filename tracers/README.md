tracers
=======

Answering the question of HOW values flow across different program components at runtime.
(Remember Jan's highway analogy)

Contains files to modify a Racket installation to produce log messages for `require/typed` contracts,
a few scripts to process this data,
and results for our gradual typing projects.


Summary
-------
High-level summary of each directory in this folder:

- `data/` the RESULTS for gradual typing projects
- `examples/` a few small projects, sanity checks to be sure the logging happens correctly
- `scripts/` code to process generated logs
- `patch-racket/` two files used to add log information when running racket

See the `README.md` file in each directory for more details.




A case for ADTs
===============
Contracts on data are overwhelmingly expensive.
Here's evidence.

Exhibit A
---------

Contract counts for worst-case projects


| project    | data contracts | % total | data checks | % total |
|------------+----------------+---------+-------------+---------|
| echo       |              - |       - | -           |       - |
| sieve      |              - |       - | -           |       - |
| morsecode  |              - |       - | -           |       - |
| mbta       |              4 |       3 | 330         |      73 |
| zordoz     |            211 |       1 | 583,892     |     100 |
| suffixtree |             20 |      30 | 494,695,557 |      97 |
| lnm        |             18 |       8 | 8,013       |      32 |
| kcfa       |             48 |     100 | 877,928,995 |     100 |
| snake      |             13 |      40 | 136,566,409 |      93 |
| tetris     |             17 |      32 | 169,689,972 |      99 |
| synth      |             12 |      19 | 24,681,549  |      47 |
| gregor     |             43 |      31 | 2,880,916   |      78 |
| quad       |            169 |      54 | 81,744      |      45 |


Exhibit B
---------

See the `unsafe/` folder.
