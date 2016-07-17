#lang typed/racket/base

(define-type Stack (Listof Integer))

(provide
 Stack
 init push)

(: init (-> Stack))
(: push (Stack Integer . -> . Stack))


(define (init)
  '())

(define (push st i)
  (cons i st))
