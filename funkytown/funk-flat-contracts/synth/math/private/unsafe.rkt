#lang racket/base

(require racket/fixnum racket/contract)

(provide (all-defined-out))

(define/contract
  unsafe-fx*
  (-> fixnum? fixnum? fixnum?)
  fx*)
(define/contract
  unsafe-fx+
  (-> fixnum? fixnum? fixnum?)
  fx+)
(define/contract
  unsafe-fxmodulo
  (-> fixnum? fixnum? fixnum?)
  fxmodulo)
(define/contract
  unsafe-vector-ref
  (-> vector? exact-nonnegative-integer? any/c)
  vector-ref)

(define/contract
  unsafe-vector-set!
  (-> (and/c vector? (not/c immutable?)) exact-nonnegative-integer? any/c void?)
  vector-set!)
