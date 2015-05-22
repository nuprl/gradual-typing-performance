#lang racket/base

(provide random-sample)

;; =============================================================================

(define (random-sample) (- (* 2.0 (random)) 1.0))
