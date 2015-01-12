#lang typed/racket/base

(require
         (for-syntax racket/base syntax/parse)
         "typed-array-struct.rkt")

(provide
 ;; Array
 Array
 Array-shape
 Array-size
 Array-unsafe-proc
 Settable-Array
 Array?
 array-default-strict
 array-strict?
 array-strictness
 build-array
 make-unsafe-array-proc
 make-unsafe-array-set-proc
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
