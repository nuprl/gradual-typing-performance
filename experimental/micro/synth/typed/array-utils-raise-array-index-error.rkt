#lang typed/racket/base

(provide raise-array-index-error)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt")

;; =============================================================================

(: raise-array-index-error (Symbol Indexes In-Indexes -> Nothing))
(define (raise-array-index-error name ds js)
  (error name "expected indexes for shape ~e; given ~e"
         (vector->list ds) js))
