#lang typed/racket/base

(provide blocks-contains?)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "block-block-eq.rkt"
  [block=? (-> Block Block Boolean)])

;; =============================================================================

;; Determine if the block is in the set of blocks.
(: blocks-contains? (-> BSet Block Boolean))
(define (blocks-contains? bs b)
  (ormap (λ: ([c : Block]) (block=? b c)) bs))
