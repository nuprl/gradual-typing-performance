#lang racket/base

(provide world-move)

(require benchmark-util
         "data-tetra.rkt"
         "data-world.rkt"
 "world-try-new-tetra.rkt"
 "tetras-tetra-move.rkt")

;; =============================================================================

;; Move the Tetra by the given X & Y displacement, but only if you can.
;; Otherwise stay put.
;(: world-move (-> Real Real World World))
(define (world-move dx dy w)
  (try-new-tetra w (tetra-move dx dy (world-tetra w))))
