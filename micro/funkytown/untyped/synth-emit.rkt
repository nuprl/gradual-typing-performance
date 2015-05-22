#lang racket/base

(provide emit)

;; -----------------------------------------------------------------------------

(require "data-array.rkt"
 (only-in "synth-signal-integer-sequence.rkt"
  signal->integer-sequence))

;; =============================================================================

;; `emit` used to write a file.
;; For now, it just converts a signal to a sequence.
(define (emit signal)
  (signal->integer-sequence signal #:gain 0.3))
