#lang typed/racket/base

(provide tetra-overlaps-blocks?)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "bset-blocks-intersect.rkt"
  [blocks-intersect (-> BSet BSet BSet)])

;; =============================================================================

;; Is the tetra on any of the blocks?
(: tetra-overlaps-blocks? (-> Tetra BSet Boolean))
(define (tetra-overlaps-blocks? t bs)
  (not (eq? '() (blocks-intersect (tetra-blocks t) bs))))
