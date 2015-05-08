#lang racket/base

(provide (struct-out Mutable-Array))

;; -----------------------------------------------------------------------------

(require "data-settable-array.rkt")

;; =============================================================================

(struct Mutable-Array Settable-Array
        (data))
