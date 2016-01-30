#lang racket/base

(provide blocks-move)

(require "data-block.rkt")
(require benchmark-util)
(require "block-block-move.rkt")

;; =============================================================================

;; Move each block by the given X & Y displacement.
;(: blocks-move (-> Real Real BSet BSet))
(define (blocks-move dx dy bs)
  (map (λ (b) (block-move dx dy b)) bs))
