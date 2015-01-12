#lang racket/base

(require racket/fixnum)

(provide (all-defined-out))

(define unsafe-fx* fx*)
(define unsafe-fx+ fx+)
(define unsafe-fxmodulo fxmodulo)
(define unsafe-vector-ref vector-ref)
(define unsafe-vector-set! vector-set!)
