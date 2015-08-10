#lang typed/racket/base

(provide array-strict?)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt")

;; =============================================================================

(: array-strict? (Array -> Boolean))
(define (array-strict? arr)
  (unbox (Array-strict? arr)))
