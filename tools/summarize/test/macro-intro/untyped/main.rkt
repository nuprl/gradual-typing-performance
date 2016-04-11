#lang racket/base
(require (for-syntax racket/base syntax/parse))
(require "x.rkt")

(define-syntax-rule (x3)
  (begin x x x (void)))

(x2)
(x3)
