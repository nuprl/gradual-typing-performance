#lang racket/base

(provide unsafe-vector->array)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
(only-in "array-utils-unsafe-array-index-value-index.rkt"
  unsafe-array-index->value-index))

;; =============================================================================

(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc Float ds (λ (j v) (vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))

;; -----------------------------------------------------------------------------
;; -- helper macros

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (λ (js)
    (ref (unsafe-array-index->value-index ds js))))

(define-syntax-rule (make-unsafe-array-set-proc A ds set!)
  (λ (js v)
    (set! (unsafe-array-index->value-index ds js) v)))
