#lang racket/base

(require typed/untyped-utils
         typed/racket/base
         (for-syntax racket/base syntax/parse)
         (except-in "typed-array-struct.rkt"
                    build-array
                    build-simple-array))

(require/untyped-contract
 (begin (require (only-in "typed-array-struct.rkt" Array)))
 "typed-array-struct.rkt"
 [build-array  (All (A) ((Vectorof Integer) ((Vectorof Index) -> A) -> (Array A)))]
 [build-simple-array  (All (A) ((Vectorof Integer) ((Vectorof Index) -> A) -> (Array A)))])

(define-syntax array? (make-rename-transformer #'Array?))
(define-syntax array-shape (make-rename-transformer #'Array-shape))
(define-syntax array-size (make-rename-transformer #'Array-size))
(define-syntax unsafe-array-proc (make-rename-transformer #'Array-unsafe-proc))

(provide
 ;; Array
 Array
 Settable-Array
 array?
 array-default-strict
 array-shape
 array-size
 array-strict?
 array-strictness
 build-array
 make-unsafe-array-proc
 make-unsafe-array-set-proc
 unsafe-array-proc
 unsafe-build-array
 unsafe-build-simple-array)

(define-syntax-rule (array-strict arr-expr)
  (let ([arr arr-expr])
    (array-strict! arr)
    arr))

(define-syntax-rule (array-default-strict arr-expr)
  (let ([arr arr-expr])
    (array-default-strict! arr)
    arr))
