#lang typed/racket/base

(provide blocks-intersect)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "bset-blocks-contains.rkt"
  [blocks-contains? (-> BSet Block Boolean)])

;; =============================================================================

;; Return the set of blocks that appear in both sets.
(: blocks-intersect (-> BSet BSet BSet))
(define (blocks-intersect bs1 bs2)
  (filter (λ: ([b : Block]) (blocks-contains? bs2 b)) bs1))
