#lang typed/racket/base

(require (only-in "../unsafe.rkt" unsafe-vector-ref unsafe-vector-set!)
         (only-in "array-struct.rkt"
                  Settable-Array
                  make-unsafe-array-proc
                  make-unsafe-array-set-proc)
         (only-in "utils.rkt" Indexes))

(provide unsafe-vector->array)

;; ===================================================================================================
;; Mutable array data type

(struct: (A) Mutable-Array Settable-Array ([data : (Vectorof A)]))

(: unsafe-vector->array (All (A) (Indexes (Vectorof A) -> (Mutable-Array A))))
(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc A ds (λ (j v) (unsafe-vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))
