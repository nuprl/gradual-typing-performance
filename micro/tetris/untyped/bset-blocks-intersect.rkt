#lang racket/base

(provide blocks-intersect)

(require "data-block.rkt")
(require benchmark-util)
(require "bset-blocks-contains.rkt")

;; =============================================================================

;; Return the set of blocks that appear in both sets.
;(: blocks-intersect (-> BSet BSet BSet))
(define (blocks-intersect bs1 bs2)
  (filter (λ (b) (blocks-contains? bs2 b)) bs1))
