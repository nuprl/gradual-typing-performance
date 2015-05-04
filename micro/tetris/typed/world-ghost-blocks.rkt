#lang typed/racket/base

(provide world-world-rotate-cw)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "world-try-new-tetra.rkt"
  [try-new-tetra (-> World Tetra World)])
(require/typed/check "tetras-tetra-change-color.rkt"
  [tetra-change-color (-> Tetra Color Tetra)])

;; =============================================================================

;; Gray blocks representing where the current tetra would land.
(: ghost-blocks (-> World BSet))
(define (ghost-blocks w)
  (tetra-blocks (tetra-change-color (world-tetra (world-jump-down w))
                                    'gray)))
