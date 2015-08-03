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
