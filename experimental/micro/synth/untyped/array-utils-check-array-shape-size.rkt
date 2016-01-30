#lang racket/base

(provide check-array-shape-size)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
(only-in "array-utils-array-shape-size.rkt"
  array-shape-size))

;; =============================================================================

(begin-encourage-inline
  (define (check-array-shape-size name ds)
    (define size (array-shape-size ds))
    (cond [(<= 0 size)  size]
          [else  (error name "array size ~e (for shape ~e) is too large (is not an Index)" size ds)]))
)
