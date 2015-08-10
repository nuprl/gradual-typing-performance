scripts
=======

Scripts to create and read traced contracts.

The main one is `trace-run.rkt`. Usage:

```
racket trace-run.rkt DIR
```

where `DIR` is a project directory, like `suffixtree/` or `quad/`.

- Runs the fully-typed version of the project
- Collects contract usage information
- Displays aggregate stats on run-time contract use

Add the `-o FILENAME` option to fill `FILENAME` with data for every contracted identifier.


Notes
-----

- The `uid` is reset often (it's a `define-for-syntax`).
  It's probably not worth using a `uid` in log messages.


Other Scripts
-------------
- `colorall.rkt` Produce colored module graphs for a few data files and existing `.tex` files
- `color-tikz.rkt` Create a colored version of a single `.tex` file using data from a traced execution.
- `list-boundaries.rkt` Summarize the boundaries in a program; processes data produced by `trace-run.rkt`.
- `runall.rkt` Apply `trace-run.rkt` to a few projects
- `tikz-parser.rkt` Copied from `paper/script/modulegraph.rkt`; get data from a line of TiKZ code.
- `trace-run.rkt` Run a project while tracking contract information. Uses the log messages produced by a modified Racket installation.
