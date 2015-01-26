#lang typed/racket

(provide f)

(define (f [x : Integer]) (add1 x))
