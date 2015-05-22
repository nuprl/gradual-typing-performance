#lang typed/racket/base

(provide tetra-move)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-tetra-adapted.rkt"
         "data-block-adapted.rkt")
(require/typed/check "bset-blocks-move.rkt"
  [blocks-move (-> Real Real BSet BSet)])

;; =============================================================================

;; Move the Tetra by the given X & Y displacement.
(: tetra-move (-> Real Real Tetra Tetra))
(define (tetra-move dx dy t)
  (tetra (posn (+ dx (posn-x (tetra-center t)))
               (+ dy (posn-y (tetra-center t))))
         (blocks-move dx dy (tetra-blocks t))))
