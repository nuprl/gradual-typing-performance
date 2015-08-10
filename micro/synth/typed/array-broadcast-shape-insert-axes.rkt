#lang typed/racket/base

(provide shape-insert-axes)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         (only-in racket/vector vector-append))

;; =============================================================================

(: shape-insert-axes (Indexes Integer -> Indexes))
(define (shape-insert-axes ds n)
  (vector-append ((inst make-vector Integer) n 1) ds))
