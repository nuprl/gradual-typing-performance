#lang typed/racket/base

(provide FOOD-RADIUS)

;; -----------------------------------------------------------------------------

(require benchmark-util)
(require/typed/check "const-segment-radius.rkt"
  [SEGMENT-RADIUS (-> Natural)])

;; =============================================================================

(: FOOD-RADIUS (-> Natural))
(define (FOOD-RADIUS) (SEGMENT-RADIUS))
