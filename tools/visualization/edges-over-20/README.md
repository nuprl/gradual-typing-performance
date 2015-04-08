edges-over-20
=============

Module graphs for each project that emphasize the number of configurations
with runtimes over 20 times larger than the best possible runtime.

- Each node is a file in the project.
  Data files ARE INCLUDED and prefixed with `base/`

- Each edge is labeled with the number of configurations
  with runtimes at least 20 times over the minimum runtime
  for which this edge is a boundary (typed/untyped OR untyped/typed).

- The width of each edge indicates the percent of all runtimes the
  label corresponds to. The exact formula is:
    `(/ (* (/ label num_configs) 100) 2)`
  In other words, divide the label by the total number of configurations,
  then multiply the result by 100 to get a percentage, then divide it by 2
  for a prettier edge width.

