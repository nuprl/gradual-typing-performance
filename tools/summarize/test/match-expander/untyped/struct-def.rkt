#lang racket/base
(provide (struct-out foo) SAMPLE-FOO)

(struct foo (x y))
(define SAMPLE-FOO (foo 4 3))
