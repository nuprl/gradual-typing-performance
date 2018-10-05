#lang info
(define collection "gtp-summarize")
(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"
               "benchmark-util" ;; local package
               "rackunit-lib"
               "rackunit-abbrevs"
               "glob"
               "trivial"
               "math-lib"
               "plot-lib"
               "pict-lib"
               "syntax-sloc"))
(define build-deps '("rackunit-lib"
                     "racket-doc"))
(define pkg-desc "Summary scripts for gradual typing performance (gtp)")
(define version "0.3")
(define pkg-authors '(ben))
(define raco-commands '(
  ("gtp-copy"
    (submod gtp-summarize/copy main)
    "Copy files using a bitstring" #f)
  ("gtp-lnm"
    (submod gtp-summarize/render-lnm main)
    "Build L-N/M plots" #f)
  ("gtp-explore"
    (submod gtp-summarize/summary main)
    "Explore dataset" #f)
  ("gtp-modulegraph"
    (submod gtp-summarize/modulegraph main)
    "Print a module graph" #f)
  ("gtp-sort"
    (submod gtp-summarize/sort-configurations main)
    "Sort configurations in a dataset" #f)
  ("gtp-status"
    (submod gtp-summarize/status main)
    "Read an active logfile, report number of configurations finished so far" #f)
  ("gtp-path"
    (submod gtp-summarize/tabulate-paths main)
    "Build a table of 'performant' paths through the lattice" #f)
  ("gtp-trace"
    (submod gtp-summarize/trace main)
    "Build a dynamic module graph for a benchmark" #f)
))
