#lang racket/base

(provide world-jump-down)

(require benchmark-util
         "data-world.rkt"
         "data-tetra.rkt")
(require "world-landed.rkt"
 "tetras-tetra-move.rkt")

;; =============================================================================

;; Take the current tetra and move it down until it lands.
;(: world-jump-down (-> World World))
(define (world-jump-down w)
  (cond [(landed? w) w]
        [else (world-jump-down (world (tetra-move 0 1 (world-tetra w))
                                      (world-blocks w)))]))
