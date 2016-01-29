#lang typed/racket/base

(provide blocks-max-y)

(require "data-block-adapted.rkt")

;; =============================================================================

;; Compute the maximum y coordinate;
;; if set is empty, return 0, the coord of the board's top edge.
(: blocks-max-y (-> BSet Real))
(define (blocks-max-y bs)
  (foldr (λ: ([b : Block]
              [n : Real])
           (max (block-y b) n)) 0 bs))
