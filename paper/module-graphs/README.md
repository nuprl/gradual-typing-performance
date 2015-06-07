Module Graphs
=============

Module dependence graphs for experiment projects.

- Running `make` generates `all-graphs.pdf`, 
- Each project has a "small" and "large" representation.
  The large one uses module names and the small uses dots.

The `-small` and `-large` tex files are just TiKZ pictures.
These pictures are all organized similarly:
- Nodes are declared in a matrix pattern. Read `00` as "row zero, column zero".
  - The first row of nodes define the spacing between columns.
  - Later rows of nodes are positioned directly below a parent in the previous row.
- Edges are declared below rows. The edges for one module are on consecutive lines.
