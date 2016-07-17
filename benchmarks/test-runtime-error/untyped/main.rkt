#lang racket/base

(require require-typed-check)
(require/typed/check "aux.rkt"
  (NUM-SLEEP String))

(define (main)
  (add1 NUM-SLEEP)
  (void))
