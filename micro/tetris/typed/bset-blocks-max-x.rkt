#lang typed/racket/base

(provide blocks-max-x)

(require "data-block-adapted.rkt")

;; =============================================================================

;; Compute the maximum x coordinate;
;; if set is empty, return 0, the coord of the board's left edge.
(: blocks-max-x (-> BSet Real))
(define (blocks-max-x bs)
  (foldr (λ: ([b : Block]
              [n : Real])
           (max (block-x b) n)) 0 bs))
