#lang racket/base

(provide world-world-rotate-cw)

(require benchmark-util
         "data-block.rkt"
         "data-tetra.rkt"
         "data-world.rkt")
(require "world-try-new-tetra.rkt")
(require "tetras-tetra-change-color.rkt")

;; =============================================================================

;; Gray blocks representing where the current tetra would land.
;(: ghost-blocks (-> World BSet))
(define (ghost-blocks w)
  (tetra-blocks (tetra-change-color (world-tetra (world-jump-down w))
                                    'gray)))
