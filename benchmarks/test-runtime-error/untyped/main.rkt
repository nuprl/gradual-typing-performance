#lang racket/base

(require benchmark-util)
(require/typed/check "aux.rkt"
  (NUM-SLEEP String))

(define (main)
  (add1 NUM-SLEEP)
  (void))
