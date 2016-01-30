#lang racket/base

(provide FOOD-RADIUS)

;; -----------------------------------------------------------------------------

(require
(only-in "const-segment-radius.rkt" SEGMENT-RADIUS))

;; =============================================================================

(define (FOOD-RADIUS) (SEGMENT-RADIUS))
