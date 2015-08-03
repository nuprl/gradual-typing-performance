data
====

Data for traced contracts for each gradual typing project.
There are 3 files for each project.

- `FILE-trace.txt` the output printed to `stdout` by `trace-run.rkt`.
  This is a high-level summary of the contracts created by the program.
- `FILE-trace.rktd` the detailed output from `trace-run.rkt`.
  This names every boundary in the program, and every value that crosses each
  boundary, and the number of times each of these values are checked.
- `FILE-trace.rktd.list` output from `list-boundaries.rkt` on `FILE-trace.rktd`.
  This is a human-readable version of the data in `FILE-trace.rktd`, emphasizing
  the total number of checks each boundary accounts for.

The other data files are:
- `top10.txt` Lists the boundaries that account for at LEAST 10% of checks for each project
