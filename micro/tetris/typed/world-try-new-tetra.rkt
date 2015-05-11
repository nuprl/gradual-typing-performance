#lang typed/racket/base

(provide try-new-tetra)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt"
         "data-world-adapted.rkt")
(require/typed/check "bset-blocks-min-x.rkt"
  [blocks-min-x (-> BSet Real)])
(require/typed/check "bset-blocks-max-x.rkt"
  [blocks-max-x (-> BSet Real)])
(require/typed/check "world-touchdown.rkt"
  [touchdown (-> World World)])
(require/typed/check "tetras-tetra-overlaps-blocks.rkt"
  [tetra-overlaps-blocks? (-> Tetra BSet Boolean)])
(require/typed/check "consts-board-width.rkt"
  [board-width Integer])

;; =============================================================================

;; Make a world with the new tetra *IF* if doesn't lie on top of some other
;; block or lie off the board. Otherwise, no change.
(: try-new-tetra (-> World Tetra World))
(define (try-new-tetra w new-tetra)
  (cond [(or (<  (blocks-min-x (tetra-blocks new-tetra)) 0)
             (>= (blocks-max-x (tetra-blocks new-tetra)) board-width)
             (tetra-overlaps-blocks? new-tetra (world-blocks w)))
         w]
        [else (world new-tetra (world-blocks w))]))
