#lang racket/base

(provide world-rotate-ccw)

(require benchmark-util
         "data-tetra.rkt"
         "data-world.rkt"
 "world-try-new-tetra.rkt"
 "tetras-tetra-rotate-ccw.rkt")

;; =============================================================================

;; Rotate the Tetra 90 degrees counterclockwise, but only if you can.
;; Otherwise stay put.
;(: world-rotate-ccw (-> World World))
(define (world-rotate-ccw w)
  (try-new-tetra w (tetra-rotate-ccw (world-tetra w))))
