#lang typed/racket/base

(provide SEGMENT-RADIUS)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "const-grid-size.rkt"
  [GRID-SIZE Natural])

;; =============================================================================

(: SEGMENT-RADIUS (-> Natural))
(define (SEGMENT-RADIUS) (quotient GRID-SIZE 2))
