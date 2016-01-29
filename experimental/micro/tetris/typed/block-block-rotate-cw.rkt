#lang typed/racket/base

(provide block-rotate-cw)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt")
(require/typed/check "block-block-rotate-ccw.rkt"
  [block-rotate-ccw (-> Posn Block Block)])

;; =============================================================================

;; Rotate the block 90 clockwise around the posn.
(: block-rotate-cw (-> Posn Block Block))
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))
