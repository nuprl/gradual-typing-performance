#lang racket/base

(provide reset!)

;; -----------------------------------------------------------------------------

(require (only-in "motion-r.rkt" r))

;; =============================================================================

(define (reset!)
  (parameterize ((current-pseudo-random-generator r))
    (random-seed 1324)))
