#lang racket/base

(provide seconds->samples)

;; -----------------------------------------------------------------------------

(require (only-in racket/math exact-round)
(only-in "synth-constants.rkt" fs))

;; =============================================================================

(define (seconds->samples s)
  (exact-round (* s fs)))
