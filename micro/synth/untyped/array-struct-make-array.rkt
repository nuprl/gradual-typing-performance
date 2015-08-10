#lang racket/base

(provide make-array)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "array-utils-check-array-shape.rkt"
  check-array-shape)
(only-in "array-struct-unsafe-build-simple-array.rkt"
  unsafe-build-simple-array))

;; =============================================================================

(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Integer)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))
