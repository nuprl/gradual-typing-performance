#lang typed/racket/base

(provide build-simple-array)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         benchmark-util)
(require/typed/check "array-utils-check-array-shape.rkt"
  [check-array-shape (-> (Vectorof Integer) (-> Nothing) Indexes)])
(require/typed/check "array-struct-unsafe-build-simple-array.rkt"
  [unsafe-build-simple-array (Indexes (Indexes -> Float) -> Array)])

;; =============================================================================

(: build-simple-array ((Vectorof Integer) (Indexes -> Float) -> Array))
(define (build-simple-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-simple-array "(Vectorof Index)" 0 ds proc)))])
    (unsafe-build-simple-array ds (λ: ([js : Indexes])
                                    (proc (vector->immutable-vector js))))))
