#lang info
(define collection "gtp-summarize")
(define deps '("base"
               "typed-racket-lib"
               "typed-racket-more"))
(define build-deps '("rackunit-lib"
                     "racket-doc"))
(define pkg-desc "")
(define version "0.2")
(define pkg-authors '(ben))
(define raco-commands '(
  ("render-lnm" (submod summarize/render-lnm main) "Build L-N/M plots" #f)
  ("gtp-summarize" (submod summarize/summary main) "Explore dataset" #f)
))
