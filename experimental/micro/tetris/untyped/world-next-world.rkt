#lang racket/base

(provide next-world)

(require benchmark-util
         "data-world.rkt")
(require "world-touchdown.rkt")
(require "tetras-tetra-move.rkt")
(require "world-landed.rkt")

;; =============================================================================

;; Step the world, either touchdown or move the tetra down on step.
;(: next-world (-> World World))
(define (next-world w)
  (cond [(landed? w) (touchdown w)]
        [else (world (tetra-move 0 1 (world-tetra w))
                     (world-blocks w))]))
