#lang racket/base

(provide blocks-rotate-ccw)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt")
(require "block-block-rotate-ccw.rkt")

;; =============================================================================

;; Rotate the blocks 90 counterclockwise around the posn.
;(: blocks-rotate-ccw (-> Posn BSet BSet))
(define (blocks-rotate-ccw c bs)
  (map (λ (b) (block-rotate-ccw c b)) bs))
