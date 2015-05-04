#lang racket/base

(provide block-rotate-ccw)

(require "data-posn.rkt"
         "data-block.rkt")

;; =============================================================================

;; Rotate the block 90 counterclockwise around the posn.
;(: block-rotate-ccw (-> Posn Block Block))
(define (block-rotate-ccw c b)
  (block (+ (posn-x c) (- (posn-y c) (block-y b)))
         (+ (posn-y c) (- (block-x b) (posn-x c)))
         (block-color b)))
