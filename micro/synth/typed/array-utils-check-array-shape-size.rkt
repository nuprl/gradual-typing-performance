#lang typed/racket/base

(provide check-array-shape-size)

;; -----------------------------------------------------------------------------

(require (only-in racket/performance-hint begin-encourage-inline)
         "type-aliases.rkt"
         benchmark-util)

(require/typed/check "array-utils-array-shape-size.rkt"
  [array-shape-size (Indexes -> Integer)])

;; =============================================================================

(begin-encourage-inline
  (: check-array-shape-size (Symbol Indexes -> Integer))
  (define (check-array-shape-size name ds)
    (define size (array-shape-size ds))
    (cond [(index? size)  size]
          [else  (error name "array size ~e (for shape ~e) is too large (is not an Index)" size ds)]))
)
