#lang typed/racket/base

(provide array-default-strict!)

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt"
         benchmark-util)
(require/typed/check "array-struct-array-strictness.rkt"
  [array-strictness (Parameterof Boolean)])

;; =============================================================================

(: array-default-strict! (Array -> Void))
(define (array-default-strict! arr)
  (define strict? (Array-strict? arr))
  (when (and (not (unbox strict?)) (array-strictness))
    ((Array-strict! arr))
    (set-box! strict? #t)))
