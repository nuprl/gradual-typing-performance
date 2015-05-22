#lang typed/racket/base

(provide world-rotate-ccw)

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "world-try-new-tetra.rkt"
  [try-new-tetra (-> World Tetra World)])
(require/typed/check "tetras-tetra-rotate-ccw.rkt"
  [tetra-rotate-ccw (-> Tetra Tetra)])

;; =============================================================================

;; Rotate the Tetra 90 degrees counterclockwise, but only if you can.
;; Otherwise stay put.
(: world-rotate-ccw (-> World World))
(define (world-rotate-ccw w)
  (try-new-tetra w (tetra-rotate-ccw (world-tetra w))))
