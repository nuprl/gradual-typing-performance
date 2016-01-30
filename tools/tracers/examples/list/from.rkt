#lang typed/racket/base

(provide nums funs idks lam)

(define nums '(1 2 3))
(define funs (list (lambda (x y) x) (lambda (x y) y)))
(define idks (list 'yes "no" 2 '()))

(: lam (-> (Listof Natural) (Listof Natural)))
(define (lam x) x)
