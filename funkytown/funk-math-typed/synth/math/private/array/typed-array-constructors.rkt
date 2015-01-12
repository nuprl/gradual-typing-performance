#lang typed/racket/base

(require (only-in "array-struct.rkt" Array unsafe-build-simple-array)
         (only-in "utils.rkt" In-Indexes check-array-shape))

(provide make-array)

(: make-array (All (A) (In-Indexes A -> (Array A))))
(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Index)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))
