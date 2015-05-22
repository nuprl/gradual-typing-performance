#lang racket/base

(provide touchdown)

(require benchmark-util
         "data-world.rkt"
         "data-tetra.rkt")
(require "aux-list-pick-random.rkt")
(require "aux-tetras.rkt")
(require "bset-blocks-union.rkt")
(require "elim-eliminate-full-rows.rkt")

;; =============================================================================

;; Add the current tetra's blocks onto the world's block list,
;; and create a new tetra.
;(: touchdown (-> World World))
(define (touchdown w)
  (world (list-pick-random tetras)
         (eliminate-full-rows (blocks-union (tetra-blocks (world-tetra w))
                                            (world-blocks w)))))
