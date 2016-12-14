#lang racket/base

(provide
  index?
  assert
)

(define (assert v p)
  (if (p v)
    v
    (raise-user-error 'assert)))

(define (index? v)
  (and (exact-nonnegative-integer? v) (< v 9999999)))
