#lang info

(define deps '("base" "glob" "typed-racket" "typed-racket-more"))
(define build-deps '("rackunit-lib"))
(define raco-commands '(
  ("gtp-run"
    (submod benchmark-run/barrier main)
    "Run benchmark (barrier-style)" #f)
  ("gtp-collect"
    (submod benchmark-run/collect main)
    "Aggregate results for a benchmark" #f)))
