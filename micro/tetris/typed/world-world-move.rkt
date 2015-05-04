#lang typed/racket/base

(provide world-move)

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "world-try-new-tetra.rkt"
  [try-new-tetra (-> World Tetra World)])
(require/typed/check "tetras-tetra-move.rkt"
  [tetra-move (-> Real Real Tetra Tetra)])

;; =============================================================================

;; Move the Tetra by the given X & Y displacement, but only if you can.
;; Otherwise stay put.
(: world-move (-> Real Real World World))
(define (world-move dx dy w)
  (try-new-tetra w (tetra-move dx dy (world-tetra w))))
