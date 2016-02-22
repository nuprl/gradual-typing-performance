#lang typed/racket/base

(define-type Stack (Listof Integer))

(provide
 Stack
 (type-out
  [init (-> Stack)]
  [push (Stack Integer . -> . Stack)]))


(define (init)
  '())

(define (push st i)
  (cons i st))
