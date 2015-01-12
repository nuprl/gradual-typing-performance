#lang racket/base

(require (only-in typed/untyped-utils define-typed/untyped-identifier)
         (rename-in "typed-array-pointwise.rkt"
                    [array-map  typed:array-map])
         (rename-in "untyped-array-pointwise.rkt"
                    [array-map  untyped:array-map]))

(define-typed/untyped-identifier array-map
  typed:array-map untyped:array-map)

(provide array-map)
