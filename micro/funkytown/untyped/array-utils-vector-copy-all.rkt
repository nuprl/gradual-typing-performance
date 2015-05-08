#lang racket/base

(provide vector-copy-all)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
(only-in "array-utils-vector-supertype-vector.rkt"
  vector->supertype-vector))

;; =============================================================================

(begin-encourage-inline
  (define (vector-copy-all js) (vector->supertype-vector js))
)
