visualization
=============

Scripts for making pictures.
- `sexp-to-tab.rkt` create a `.tab` file from a `.rktd` file.
- `grouping.py` create a histogram of runtimes
- `shortest-path.py` summarize paths in the lattice

sexp-to-tab.rkt
---------------

Usage: `sexp-to-tab.rkt FILE.rktd`

Converts a vector s-expression (i.e., the output of the `run.rkt` script) into the .tab file format.
In the output, data for each configuration is on a new line.


grouping.py
-----------

Usage: `grouping.py FILE.tab`

Creates a histogram from the runtimes in `FILE.tab`.
Histogram bins are spaced evenly between the minimum and maximum runtimes of any configuration.
The number of BINS is hard-coded near the bottom of the file.


shortest-path.py
----------------

Usage: `shortest-path.py FILE.tab`

Creates the module lattice from the data in `FILE.tab`.
First prints the shortest path (by SUM) from the fully-untyped configuration up to the typed configuration.
Next creates histograms of all paths from untyped to typed.
The histograms are partitioned as in `grouping.py`, but weights are computed using a hard-coded measure.
The number of bins is hard-coded near the bottom of the file.

The measures we currently use are:
- SUM: a path's weight is the sum of its edges' weights
- MAX: a path's weight is the maximum of its edges' weights
- MIN: like MAX, but using minumum weight

