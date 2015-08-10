#lang racket/base

(provide sawtooth-wave)

;; -----------------------------------------------------------------------------

(require (only-in "synth-make-sawtooth-wave.rkt" make-sawtooth-wave))

;; =============================================================================

(define sawtooth-wave         (make-sawtooth-wave 1.0))
