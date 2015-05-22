#lang racket/base

(provide freq->sample-period)

;; -----------------------------------------------------------------------------

(require (only-in racket/math exact-round)
(only-in "synth-constants.rkt" fs))

;; =============================================================================

(define (freq->sample-period freq)
  (exact-round (/ fs freq)))
