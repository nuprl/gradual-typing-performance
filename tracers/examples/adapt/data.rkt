#lang racket/base

(provide (struct-out foo) val foo1)

(struct foo (x y z))
(define val 42)
(define foo1 (foo (lambda (x) x) "16" '(1 2 3)))
