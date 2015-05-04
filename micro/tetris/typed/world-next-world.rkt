#lang typed/racket/base

(provide next-world)

(require benchmark-util
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "world-touchdown.rkt"
  [touchdown (-> World World)])
(require/typed/check "tetras-tetra-move.rkt"
  [tetra-move (-> Real Real Tetra Tetra)])
(require/typed/check "world-landed.rkt"
  [landed? (-> World Boolean)])

;; =============================================================================

;; Step the world, either touchdown or move the tetra down on step.
(: next-world (-> World World))
(define (next-world w)
  (cond [(landed? w) (touchdown w)]
        [else (world (tetra-move 0 1 (world-tetra w))
                     (world-blocks w))]))
