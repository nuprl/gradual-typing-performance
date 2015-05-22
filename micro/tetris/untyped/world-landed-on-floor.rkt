#lang racket/base

(provide landed-on-floor?)

(require benchmark-util
         "data-block.rkt"
         "data-world.rkt"
         "data-tetra.rkt")
(require "tetras-tetra-overlaps-blocks.rkt")
(require "bset-blocks-max-y.rkt")
(require "consts-board-height.rkt")

;; =============================================================================

;; Has the current tetra landed on the floor?
(define (landed-on-floor? w)
  (= (blocks-max-y (tetra-blocks (world-tetra w)))
     (sub1 board-height)))
