#lang typed/racket/base

(provide landed-on-blocks?)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-world-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "tetras-tetra-overlaps-blocks.rkt"
  [tetra-overlaps-blocks? (-> Tetra BSet Boolean)])
(require/typed/check "tetras-tetra-move.rkt"
  [tetra-move (-> Real Real Tetra Tetra)])

;; =============================================================================

;; Has the current tetra landed on blocks?
;; I.e., if we move the tetra down 1, will it touch any existing blocks?
(: landed-on-blocks? (-> World Boolean))
(define (landed-on-blocks? w)
  (tetra-overlaps-blocks? (tetra-move 0 1 (world-tetra w))
                          (world-blocks w)))
