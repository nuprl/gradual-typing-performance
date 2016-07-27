#lang info

(define deps '("base" "glob" "typed-racket" "typed-racket-more" "math-lib" "summarize"))
(define build-deps '("rackunit-lib" "rackunit-abbrevs"))
(define raco-commands '(
  ("gtp-run"
    (submod benchmark-run/barrier main)
    "Run benchmark (barrier-style)" #f)
  ("gtp-collect"
    (submod benchmark-run/collect main)
    "Aggregate results for a benchmark" #f)))
