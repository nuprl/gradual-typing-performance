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

;; morsecode
(let ()
  (printf "Processing 'morsecode'\n")
  (define trace "tracers/data/morse-code-trace.rktd")
  (define mg    "paper/module-graphs/morsecode.tex")
  (define out   "tracers/colored/morse-code-color.tex")
  (system (format "racket tracers/scripts/color-tikz.rkt -o ~a ~a ~a" out trace mg))
  (void))
;; zordoz
(let ()
  (printf "Processing 'zordoz'\n")
  (define trace "tracers/data/zordoz2-trace.rktd")
  (define mg    "paper/module-graphs/zordoz.tex")
  (define out   "tracers/colored/zordoz-color.tex")
  (system (format "racket tracers/scripts/color-tikz.rkt -o ~a ~a ~a" out trace mg))
  (void))
;; the rest
(for ([pn (in-list data)])
  (printf "Processing '~a'\n" pn)
  (define trace (format "tracers/data/~a-trace.rktd" pn))
  (define mg    (format "paper/module-graphs/~a.tex" pn))
  (define out   (format "tracers/colored/~a-color.tex" pn))
  (system (format "racket tracers/scripts/color-tikz.rkt -o ~a ~a ~a" out trace mg)))

