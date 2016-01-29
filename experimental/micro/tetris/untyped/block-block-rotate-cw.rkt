#lang racket/base

(provide block-rotate-cw)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt")
(require "block-block-rotate-ccw.rkt")

;; =============================================================================

;; Rotate the block 90 clockwise around the posn.
;(: block-rotate-cw (-> Posn Block Block))
(define (block-rotate-cw c b)
  (block-rotate-ccw c (block-rotate-ccw c (block-rotate-ccw c b))))
