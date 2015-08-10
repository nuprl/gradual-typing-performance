#lang racket/base

(provide array-broadcast)

;; -----------------------------------------------------------------------------

(require
         "data-array.rkt"
         (only-in racket/fixnum fx<=))

(require (only-in "array-struct-array-strict.rkt"
  array-strict?)
 (only-in "array-struct-array-default-strict.rkt"
  array-default-strict!)
 (only-in "array-broadcast-shift-stretch-axes.rkt"
  shift-stretch-axes))

;; =============================================================================

;(: array-broadcast (-> Array Indexes Array))
(define (array-broadcast arr ds)
  (cond [(equal? ds (Array-shape arr))  arr]
        [else  (define new-arr (shift-stretch-axes arr ds))
               (if (or (array-strict? arr) ((Array-size new-arr) . fx<= . (Array-size arr)))
                   new-arr
                   (begin (array-default-strict! new-arr) new-arr))]))
