#lang racket/base
(require (for-syntax racket/base syntax/parse))
(provide x x2)

(define x 4)

(define-syntax (x2 stx)
  (syntax-parse stx
   [(_)
    #'(begin x x (void))]))
