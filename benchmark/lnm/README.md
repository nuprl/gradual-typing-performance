lnm-plot
========

Scripts implementing the data analysis for the paper.
Computes a set of `L-N/M` plots for a dataset+modulegraph and converts the data
to a spreadsheet.

Summary
-------
- `bitstring.rkt` Helpers for working with binary-string representations of natural numbers
- `lnm-plot.rkt` Create the actual `L-N/M` plots using Racket's `plot` library
- `main.rkt` Entry point for the benchmark
- `modulegraph.rkt` Parser for `.tex` modulegraphs and data representation of adjacency lists
- `spreadsheet.rkt` Convert a data file to a spreadsheet
- `summary.rkt` Data representation of experimental results
