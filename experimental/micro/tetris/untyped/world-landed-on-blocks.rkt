#lang racket/base

(provide landed-on-blocks?)

(require benchmark-util
         "data-block.rkt"
         "data-world.rkt"
         "data-tetra.rkt")
(require "tetras-tetra-overlaps-blocks.rkt")
(require "tetras-tetra-move.rkt")

;; =============================================================================

;; Has the current tetra landed on blocks?
;; I.e., if we move the tetra down 1, will it touch any existing blocks?
(define (landed-on-blocks? w)
  (tetra-overlaps-blocks? (tetra-move 0 1 (world-tetra w))
                          (world-blocks w)))
