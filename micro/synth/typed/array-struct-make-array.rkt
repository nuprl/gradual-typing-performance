#lang typed/racket/base

(provide make-array)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         benchmark-util)
(require/typed/check "array-utils-check-array-shape.rkt"
  [check-array-shape (-> (Vectorof Integer) (-> Nothing) Indexes)])
(require/typed/check "array-struct-unsafe-build-simple-array.rkt"
  [unsafe-build-simple-array (Indexes (Indexes -> Float) -> Array)])

;; =============================================================================

(: make-array ((Vectorof Integer) Float -> Array))
(define (make-array ds v)
  (let ([ds  (check-array-shape
              ds (λ () (raise-argument-error 'make-array "(Vectorof Integer)" 0 ds v)))])
    (unsafe-build-simple-array ds (λ (js) v))))
