#lang racket/base

(provide array-strict?)

;; -----------------------------------------------------------------------------

(require "data-array.rkt")

;; =============================================================================

(define (array-strict? arr)
  (unbox (Array-strict? arr)))
