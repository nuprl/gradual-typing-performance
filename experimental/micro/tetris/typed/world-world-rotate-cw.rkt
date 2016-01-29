#lang typed/racket/base

(provide world-rotate-cw)

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "world-try-new-tetra.rkt"
  [try-new-tetra (-> World Tetra World)])
(require/typed/check "tetras-tetra-rotate-cw.rkt"
  [tetra-rotate-cw (-> Tetra Tetra)])

;; =============================================================================

;; Rotate the Tetra 90 degrees clockwise, but only if you can.
;; Otherwise stay put.
(: world-rotate-cw (-> World World))
(define (world-rotate-cw w)
  (try-new-tetra w (tetra-rotate-cw (world-tetra w))))
