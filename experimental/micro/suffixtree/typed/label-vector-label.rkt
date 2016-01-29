#lang typed/racket/base

(provide vector->label)

;; -----------------------------------------------------------------------------

(require "data-label-adapted.rkt")

;; =============================================================================

;; vector->label vector
;; Constructs a new label from the input vector.
(: vector->label (-> (Vectorof (U Char Symbol)) label))
(define (vector->label vector)
  (label (vector->immutable-vector vector)
         0 (vector-length vector)))
