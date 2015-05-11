#lang racket/base

(provide SEGMENT-RADIUS)

;; -----------------------------------------------------------------------------

(require
(only-in "const-grid-size.rkt" GRID-SIZE))

;; =============================================================================

(define (SEGMENT-RADIUS) (quotient GRID-SIZE 2))
