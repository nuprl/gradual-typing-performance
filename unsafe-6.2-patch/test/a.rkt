#lang racket/base

(struct foo (x y))
(define a-foo (foo 1 2))
(provide (struct-out foo) a-foo)
