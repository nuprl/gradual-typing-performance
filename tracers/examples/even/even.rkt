#lang racket/base

(provide even?)

(define (even? odd? x)
  (or (zero? x) (odd? (sub1 x))))
