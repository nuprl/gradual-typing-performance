#lang typed/racket/base

(provide (struct-out $penalty))

;; -----------------------------------------------------------------------------

(require
  benchmark-util
  "../base/core-types.rkt")

(require/typed/check "penalty-struct.rkt"
  [#:struct $penalty ([hyphens : Nonnegative-Integer]
                      [width : Value-Type])])

;; =============================================================================

(define-type Value-Type Float)
