#lang racket/base

(require 
         (for-syntax racket/base syntax/parse)
         "typed-array-struct.rkt")

(define-syntax array? (make-rename-transformer #'Array?))
(define-syntax array-shape (make-rename-transformer #'Array-shape))
(define-syntax array-size (make-rename-transformer #'Array-size))
(define-syntax unsafe-array-proc (make-rename-transformer #'Array-unsafe-proc))

(provide
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
