#lang racket/base

(provide build-simple-array)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "array-utils-check-array-shape.rkt"
  check-array-shape)
(only-in "array-struct-unsafe-build-simple-array.rkt"
  unsafe-build-simple-array))

;; =============================================================================

(: build-simple-array ((Vectorof Integer) (Indexes -> Float) -> Array))
(define (build-simple-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-simple-array "(Vectorof Index)" 0 ds proc)))])
    (unsafe-build-simple-array ds (λ ([js])
                                    (proc (vector->immutable-vector js))))))
