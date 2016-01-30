#lang typed/racket/base

(provide random-sample)

;; =============================================================================

(: random-sample (-> Float))
(define (random-sample) (- (* 2.0 (random)) 1.0))
