#lang racket/base

(require (rename-in "untyped-array-pointwise.rkt"
                    [array-map  untyped:array-map]))

(define array-map untyped:array-map)

(provide array-map)
