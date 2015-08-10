#lang racket/base

(provide shape-insert-axes)

;; -----------------------------------------------------------------------------

(require
         (only-in racket/vector vector-append))

;; =============================================================================

(define (shape-insert-axes ds n)
  (vector-append (make-vector n 1) ds))
