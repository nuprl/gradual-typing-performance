#lang racket/base

(provide blocks-min-x)

(require benchmark-util
         "data-block.rkt")
(require "consts-board-width.rkt")

;; =============================================================================

;; Compute the minimum x coordinate;
;; if set is empty, return the coord of the board's right edge.
;(: blocks-min-x (-> BSet Real))
(define (blocks-min-x bs)
  (foldr (λ (b 
              n)
           (min (block-x b) n)) board-width bs))
