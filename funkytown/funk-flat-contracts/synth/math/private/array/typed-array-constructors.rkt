#lang racket/base

(require (only-in "array-struct.rkt" array? unsafe-build-simple-array)
         (only-in "utils.rkt" check-array-shape)
         racket/contract)

(provide make-array)

(define/contract
  (make-array ds v)
  (-> vector? flonum? array?)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Index)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))
