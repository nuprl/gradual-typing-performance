#lang racket/base

(provide
  face?
  bulls?
  assert
)

;; -----------------------------------------------------------------------------

(define (assert v p?)
  (if (p? v) v (raise-user-error 'assert "(~a ~a)" p? v)))

(define (face? f)
  (and (exact-nonnegative-integer? f) (< 0 f) (< f 105)))

(define (bulls? b)
  (and (exact-nonnegative-integer? b) (< 1 b) (< b 8)))
