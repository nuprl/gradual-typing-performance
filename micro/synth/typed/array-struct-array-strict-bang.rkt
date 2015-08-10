#lang typed/racket/base

(provide array-strict!)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt")

;; =============================================================================

(: array-strict! (Array -> Void))
(define (array-strict! arr)
  (define strict? (Array-strict? arr))
  (unless (unbox strict?)
    ((Array-strict! arr))
    (set-box! strict? #t)))
