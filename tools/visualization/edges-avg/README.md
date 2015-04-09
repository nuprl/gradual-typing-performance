edges-avg
=========

Module graphs showing the average runtime of configurations where
each edge is a boundary.

- Each node is a file in the project

- Edges are labeled with the average runtime of configurations where
  this edge is a boundary (typed/untyped OR untyped/typed)

- Edge widths are in proportion to the number of times greater
  the label runtime is compared to the minimum runtime.

Aggregate Runtimes
------------------

+------------+----------+----------+
| Project    | Min Time | Max Time |
+------------+----------+----------+
| echo       |     1888 |     2117 |
| funkytown  |      334 |    14363 |
| mbta       |   202842 |   318210 |
| morse-code |      189 |      335 |
| sieve      |     6810 |   699519 |
| suffixtree |     2003 |   300812 |
| tetris     |      482 |      698 |
| zordoz     |      576 |     2126 |
+------------+----------+----------+

