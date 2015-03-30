#lang typed/racket

(require "../base/c.rkt")

(provide f)

(define (f [x : Integer])
  (sleep 0.01)
  (mac x))
