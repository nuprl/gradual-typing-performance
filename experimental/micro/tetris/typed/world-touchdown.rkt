#lang typed/racket/base

(provide touchdown)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-world-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "aux-list-pick-random.rkt"
  [list-pick-random (-> (Listof Tetra) Tetra)])
(require/typed/check "aux-tetras.rkt"
  [tetras (Listof Tetra)])
(require/typed/check "bset-blocks-union.rkt"
  [blocks-union (-> BSet BSet BSet)])
(require/typed/check "elim-eliminate-full-rows.rkt"
  [eliminate-full-rows (-> BSet BSet)])

;; =============================================================================

;; Add the current tetra's blocks onto the world's block list,
;; and create a new tetra.
(: touchdown (-> World World))
(define (touchdown w)
  (world (list-pick-random tetras)
         (eliminate-full-rows (blocks-union (tetra-blocks (world-tetra w))
                                            (world-blocks w)))))
