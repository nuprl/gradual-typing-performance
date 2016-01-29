diagnose
===

Tools for diagnosing problems in a program.

RIGHT NOW, serves as a prototype dataframe-viewing library.
- Create a file using the format in `data/README.md`
- Fire up `raco gtp-diagnose data/FILE.rktd`
- Type `(help)` to get started; the entire `diagnose.rkt` file is open in the REPL

The basic workflow is to add some rows, look for a pattern,
 add some hlines to select configs in the pattern, then use `all-coverings` to find predicates
 that cover all (or almost-all) the configs.

More coming soon.
