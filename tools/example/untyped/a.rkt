#lang racket

(provide f)

(define (f x)
  (sleep 0.01)
  (add1 x))
