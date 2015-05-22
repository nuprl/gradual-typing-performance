#lang racket/base

(provide world-rotate-cw)

(require benchmark-util
         "data-tetra.rkt"
         "data-world.rkt"
 "world-try-new-tetra.rkt"
 "tetras-tetra-rotate-cw.rkt")

;; =============================================================================

;; Rotate the Tetra 90 degrees clockwise, but only if you can.
;; Otherwise stay put.
;(: world-rotate-cw (-> World World))
(define (world-rotate-cw w)
  (try-new-tetra w (tetra-rotate-cw (world-tetra w))))
