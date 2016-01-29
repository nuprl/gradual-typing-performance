#lang racket/base

(provide blocks-contains?)

(require "data-block.rkt")
(require benchmark-util)
(require "block-block-eq.rkt")

;; =============================================================================

;; Determine if the block is in the set of blocks.
;(: blocks-contains? (-> BSet Block Boolean))
(define (blocks-contains? bs b)
  (ormap (λ (c) (block=? b c)) bs))
