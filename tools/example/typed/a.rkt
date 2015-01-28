#lang typed/racket

(provide f)

(define (f [x : Integer])
  (sleep 0.01)
  (add1 x))
