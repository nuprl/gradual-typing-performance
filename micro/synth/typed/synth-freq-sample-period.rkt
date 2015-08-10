#lang typed/racket/base

(provide freq->sample-period)

;; -----------------------------------------------------------------------------

(require (only-in racket/math exact-round)
         benchmark-util)
(require/typed/check "synth-constants.rkt"
  [fs Natural])

;; =============================================================================

(: freq->sample-period (-> Float Integer))
(define (freq->sample-period freq)
  (exact-round (/ fs freq)))
