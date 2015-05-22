#lang typed/racket/base

(provide build-array)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         benchmark-util)
(require/typed/check "array-utils-check-array-shape.rkt"
  [check-array-shape (-> (Vectorof Integer) (-> Nothing) Indexes)])
(require/typed/check "array-struct-array-default-strict.rkt"
  [array-default-strict! (Array -> Void)])
(require/typed/check "array-struct-unsafe-build-array.rkt"
  [unsafe-build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array)])

;; =============================================================================

(: build-array ((Vectorof Integer) ((Vectorof Integer) -> Float) -> Array))
(define (build-array ds proc)
  (let ([ds  (check-array-shape
              ds (lambda () (raise-argument-error 'build-array "(Vectorof Integer)" 0 ds proc)))])
    (define arr
      (unsafe-build-array ds (λ: ([js : (Vectorof Integer)])
                               (proc (vector->immutable-vector js)))))
    (array-default-strict! arr)
    arr))
