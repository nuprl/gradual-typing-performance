#lang racket/base

(require (only-in "../unsafe.rkt" unsafe-vector-ref unsafe-vector-set!)
         (only-in "array-struct.rkt"
                  Settable-Array
                  make-unsafe-array-proc
                  make-unsafe-array-set-proc))

(provide unsafe-vector->array)

;; ===================================================================================================
;; Mutable array data type

(struct Mutable-Array Settable-Array ([data ]))

(define (unsafe-vector->array ds vs)
  (define proc (make-unsafe-array-proc ds (λ (j) (unsafe-vector-ref vs j))))
  (define set-proc (make-unsafe-array-set-proc A ds (λ (j v) (unsafe-vector-set! vs j v))))
  (Mutable-Array ds (vector-length vs) (box #t) void proc set-proc vs))
