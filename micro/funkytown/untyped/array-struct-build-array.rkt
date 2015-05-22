#lang racket/base

(provide build-array)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "array-utils-check-array-shape.rkt"
  check-array-shape)
 (only-in "array-struct-array-default-strict.rkt"
  array-default-strict!)
 (only-in "array-struct-unsafe-build-array.rkt"
  unsafe-build-array))

;; =============================================================================

(define (build-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-array "(Vectorof Integer)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (lambda (js)
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))
