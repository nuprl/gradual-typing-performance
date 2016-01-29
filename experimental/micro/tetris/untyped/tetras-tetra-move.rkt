#lang racket/base

(provide tetra-move)

(require benchmark-util
         "data-posn.rkt"
         "data-tetra.rkt"
         "data-block.rkt")
(require "bset-blocks-move.rkt")

;; =============================================================================

;; Move the Tetra by the given X & Y displacement.
;(: tetra-move (-> Real Real Tetra Tetra))
(define (tetra-move dx dy t)
  (tetra (posn (+ dx (posn-x (tetra-center t)))
               (+ dy (posn-y (tetra-center t))))
         (blocks-move dx dy (tetra-blocks t))))
