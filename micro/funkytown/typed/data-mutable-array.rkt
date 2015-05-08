#lang typed/racket/base

(provide (struct-out Mutable-Array))

;; -----------------------------------------------------------------------------

(require "data-settable-array-adapted.rkt")

;; =============================================================================

(struct Mutable-Array Settable-Array
        ([data : (Vectorof Float)]))
