#lang racket/base

(provide blocks-rotate-cw)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt")
(require "block-block-rotate-cw.rkt")

;; =============================================================================

;; Rotate the blocks 90 clockwise around the posn.
;(: blocks-rotate-cw (-> Posn BSet BSet))
(define (blocks-rotate-cw c bs)
  (map (λ (b) (block-rotate-cw c b)) bs))
