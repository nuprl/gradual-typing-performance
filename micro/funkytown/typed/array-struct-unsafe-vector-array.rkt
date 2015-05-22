#lang typed/racket/base

(provide unsafe-vector->array)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         benchmark-util)

(require/typed/check "array-utils-unsafe-array-index-value-index.rkt"
  [unsafe-array-index->value-index (Indexes Indexes -> Integer)])

;; =============================================================================

(: unsafe-vector->array (Indexes (Vectorof Float) -> Mutable-Array))
(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc Float ds (λ (j v) (vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))

;; -----------------------------------------------------------------------------
;; -- helper macros

(define-syntax-rule (make-unsafe-array-proc ds ref)
  (λ: ([js : Indexes])
    (ref (unsafe-array-index->value-index ds js))))

(define-syntax-rule (make-unsafe-array-set-proc A ds set!)
  (λ: ([js : Indexes] [v : A])
    (set! (unsafe-array-index->value-index ds js) v)))
