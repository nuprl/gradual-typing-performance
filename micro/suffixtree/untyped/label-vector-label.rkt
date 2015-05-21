#lang racket/base

(provide vector->label)

;; -----------------------------------------------------------------------------

(require "data-label.rkt")

;; =============================================================================

;; vector->label vector
;; Constructs a new label from the input vector.
(define (vector->label vector)
  (label (vector->immutable-vector vector)
         0 (vector-length vector)))
