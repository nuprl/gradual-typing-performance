#lang typed/racket/base

(require require-typed-check)
(require/typed/check "aux.rkt"
  (NUM-SLEEP String))

(define (main)
  (add1 (assert NUM-SLEEP integer?))
  (void))
