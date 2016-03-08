#lang racket/base

(module t typed/racket/base
  (define-type C (Pairof Nonnegative-Real Real))
  ;; C = (Distance from origin, Radians)

  (: complex-* (C C -> C))
  (define (complex-* c1 c2)
    (cons (* (car c1) (car c2))
          (+ (cdr c1) (cdr c2))))

  (provide complex-*))

(module u racket/base
  (require (submod ".." t))

  'TODO
)
(require 'u)
