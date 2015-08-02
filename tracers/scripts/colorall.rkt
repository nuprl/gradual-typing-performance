#lang racket/base

;; Create colored versions of a few module graphs
;; Should be run from the root of the `gradual-typing-performance` folder

(require
  (only-in racket/system system)
)

(define data '(
  "echo"
  "sieve"
  ;"morse-code"
  "mbta"
  ;"zordoz2"
  "suffixtree"
  "lnm"
  "kcfa"
  "snake"
  "tetris"
  "funkytown"
  "gregor"
  "quad"
))

(for ([pn (in-list data)])
  (printf "Processing '~a'\n" pn)
  (define trace (format "~a-trace.rktd" pn))
  (define mg    (format "paper/module-graphs/~a.tex" pn))
  (define out   (format "~a-color.tex" pn))
  (system (format "racket tracers/scripts/color-tikz.rkt -o ~a ~a ~a" out trace mg)))

