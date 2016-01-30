#lang typed/racket/base

(provide tetra-change-color)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "bset-blocks-change-color.rkt"
  [blocks-change-color (-> BSet Color BSet)])

;; =============================================================================

;; Change the color of the given tetra.
(: tetra-change-color (-> Tetra Color Tetra))
(define (tetra-change-color t c)
  (tetra (tetra-center t)
         (blocks-change-color (tetra-blocks t) c)))
