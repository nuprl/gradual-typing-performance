#lang typed/racket/base

(require/typed "aux.rkt"
  (NUM-SLEEP String))

(define (main)
  (add1 (assert NUM-SLEEP integer?))
  (void))
