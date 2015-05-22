#lang typed/racket/base

(provide seconds->samples)

;; -----------------------------------------------------------------------------

(require (only-in racket/math exact-round)
         benchmark-util)
(require/typed/check "synth-constants.rkt"
  [fs Natural])

;; =============================================================================

(: seconds->samples (-> Float Integer))
(define (seconds->samples s)
  (exact-round (* s fs)))
