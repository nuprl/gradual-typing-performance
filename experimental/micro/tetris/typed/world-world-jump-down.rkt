#lang typed/racket/base

(provide world-jump-down)

(require benchmark-util
         "data-world-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "world-landed.rkt"
  [landed? (-> World Boolean)])
(require/typed/check "tetras-tetra-move.rkt"
  [tetra-move (-> Real Real Tetra Tetra)])

;; =============================================================================

;; Take the current tetra and move it down until it lands.
(: world-jump-down (-> World World))
(define (world-jump-down w)
  (cond [(landed? w) w]
        [else (world-jump-down (world (tetra-move 0 1 (world-tetra w))
                                      (world-blocks w)))]))
