#lang racket/base

;; Run a few projects and collect their traced-contract output.
;; Run this script from the "gradual-typing-performance" directory root

(require (only-in racket/system system))

(define dirs '(
  "echo"
  "sieve"
  "morse-code"
  "mbta"
  "zordoz2"
  "suffixtree"
  "lnm"
  "kcfa"
  "snake"
  "tetris"
  "funkytown"
  "gregor"
  "quad"
))

(module+ main
  (for ([d (in-list dirs)])
    (display d)
    (display "\t")
    (display (system (format "racket tracers/scripts/trace-run.rkt -o ~a-trace.rktd ~a > ~a-trace.txt" d d d)))
    (newline)))
