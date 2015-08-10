#lang racket/base

(provide array-default-strict!)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
(only-in "array-struct-array-strictness.rkt"
  array-strictness))

;; =============================================================================

(define (array-default-strict! arr)
  (define strict? (Array-strict? arr))
  (when (and (not (unbox strict?)) (array-strictness))
    ((Array-strict! arr))
    (set-box! strict? #t)))
