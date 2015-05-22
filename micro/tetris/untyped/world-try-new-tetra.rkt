#lang racket/base

(provide try-new-tetra)

(require benchmark-util
         "data-tetra.rkt"
         "data-world.rkt")
(require "bset-blocks-min-x.rkt")
(require "bset-blocks-max-x.rkt")
(require "world-touchdown.rkt")
(require "tetras-tetra-overlaps-blocks.rkt")
(require "consts-board-width.rkt")

;; =============================================================================

;; Make a world with the new tetra *IF* if doesn't lie on top of some other
;; block or lie off the board. Otherwise, no change.
;(: try-new-tetra (-> World Tetra World))
(define (try-new-tetra w new-tetra)
  (cond [(or (<  (blocks-min-x (tetra-blocks new-tetra)) 0)
             (>= (blocks-max-x (tetra-blocks new-tetra)) board-width)
             (tetra-overlaps-blocks? new-tetra (world-blocks w)))
         w]
        [else (world new-tetra (world-blocks w))]))
