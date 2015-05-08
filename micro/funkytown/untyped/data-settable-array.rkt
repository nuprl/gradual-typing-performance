#lang racket/base

(provide (struct-out Settable-Array))

;; -----------------------------------------------------------------------------

(require "data-array.rkt")

;; =============================================================================

(struct Settable-Array Array
        (set-proc))
