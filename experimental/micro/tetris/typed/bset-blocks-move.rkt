#lang typed/racket/base

(provide blocks-move)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "block-block-move.rkt"
  [block-move (-> Real Real Block Block)])

;; =============================================================================

;; Move each block by the given X & Y displacement.
(: blocks-move (-> Real Real BSet BSet))
(define (blocks-move dx dy bs)
  (map (λ: ([b : Block]) (block-move dx dy b)) bs))
