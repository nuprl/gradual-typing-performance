#lang racket/base

(require (only-in "array-struct.rkt" unsafe-build-simple-array)
         (only-in "utils.rkt" check-array-shape))

(provide make-array)

(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Index)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))
