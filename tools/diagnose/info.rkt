#lang info
(define collection "gtp-diagnose")
(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"
               "benchmark-util"
               "glob"
               "math-lib"
               "plot-lib"))
(define build-deps '("rackunit-lib"
                     "racket-doc"))
(define pkg-desc "")
(define version "0.1")
(define pkg-authors '(ben))
(define raco-commands '(
  ("gtp-diagnose"
   (submod gtp-diagnose/diagnose main)
   "Interactively plot chunks of a data frame" #f)
))
