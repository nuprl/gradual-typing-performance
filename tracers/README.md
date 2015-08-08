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

Average runtime of projects, with and without a data boundary.
The meaning of "data boundary" changes for each project, hence the third column.
If any file in the third column sits on a typed/untyped boundary, it counts as a data boundary.

TODO add "anything else" column

| project    |        no |       yes | pct diff | data files                  |
|------------+-----------+-----------+----------+-----------------------------|
| echo       |         - |         - |        - |                             |
| sieve      |         - |         - |        - |                             |
| morsecode  |         - |         - |        - |                             |
| mbta       |           |           |          |                             |
| zordoz     |           |           |          |                             |
| suffixtree | 114455.20 | 165263.70 |    44.39 | data                        |
| lnm        |           |           |          |                             |
| kcfa       |           |           |        - |                             |
| snake      |  16291.37 |  20551.85 |    26.15 | data                        |
| tetris     |  15222.88 |  24970.38 |    64.03 | data                        |
| synth      |   8831.96 |  10626.33 |    20.32 | data                        |
| gregor     |   1146.31 |   1814.99 |    58.33 | core-structs                |
| gregor     |   1391.55 |   1811.41 |    30.17 | gregor-structs              |
| gregor     |    963.44 |   1810.18 |    87.89 | core-structs gregor-structs |
| quad       |   6150.04 |   6185.95 |     0.58 | ocm                         |


