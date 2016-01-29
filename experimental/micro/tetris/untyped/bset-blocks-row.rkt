#lang racket/base

(provide blocks-row)

(require "data-block.rkt")

;; =============================================================================

;; Return the set of blocks in the given row.
;(: blocks-row (-> BSet Real BSet))
(define (blocks-row bs i)
  (filter (λ (b) (= i (block-y b))) bs))
