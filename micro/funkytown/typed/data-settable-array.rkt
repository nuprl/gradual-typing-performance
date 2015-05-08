#lang typed/racket/base

(provide (struct-out Settable-Array))

;; -----------------------------------------------------------------------------

(require "data-array-adapted.rkt")

;; =============================================================================

(struct Settable-Array Array
        ([set-proc : ((Vectorof Integer) Float -> Void)]))
