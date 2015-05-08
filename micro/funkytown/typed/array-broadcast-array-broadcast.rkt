#lang typed/racket/base

(provide array-broadcast)

;; -----------------------------------------------------------------------------

(require "type-aliases.rkt"
         "data-array-adapted.rkt"
         (only-in racket/fixnum fx<=)
         benchmark-util)

(require/typed/check "array-struct-array-strict.rkt"
  [array-strict? (Array -> Boolean)])
(require/typed/check "array-struct-array-default-strict.rkt"
  [array-default-strict! (Array -> Void)])
(require/typed/check "array-broadcast-shift-stretch-axes.rkt"
  [shift-stretch-axes (-> Array Indexes Array)])

;; =============================================================================

(: array-broadcast (-> Array Indexes Array))
(define (array-broadcast arr ds)
  (cond [(equal? ds (Array-shape arr))  arr]
        [else  (define new-arr (shift-stretch-axes arr ds))
               (if (or (array-strict? arr) ((Array-size new-arr) . fx<= . (Array-size arr)))
                   new-arr
                   (begin (array-default-strict! new-arr) new-arr))]))
