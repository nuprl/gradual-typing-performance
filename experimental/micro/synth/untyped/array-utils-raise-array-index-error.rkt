#lang racket/base

(provide raise-array-index-error)

;; -----------------------------------------------------------------------------
;; =============================================================================

(define (raise-array-index-error name ds js)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))
