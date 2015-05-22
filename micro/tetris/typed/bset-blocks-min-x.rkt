#lang typed/racket/base

(provide blocks-min-x)

(require benchmark-util
         "data-block-adapted.rkt")
(require/typed/check "consts-board-width.rkt"
  [board-width Integer])

;; =============================================================================

;; Compute the minimum x coordinate;
;; if set is empty, return the coord of the board's right edge.
(: blocks-min-x (-> BSet Real))
(define (blocks-min-x bs)
  (foldr (λ: ([b : Block]
              [n : Real])
           (min (block-x b) n)) board-width bs))
