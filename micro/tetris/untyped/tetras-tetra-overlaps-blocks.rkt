#lang racket/base

(provide tetra-overlaps-blocks?)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt"
         "data-tetra.rkt")
(require "bset-blocks-intersect.rkt")

;; =============================================================================

;; Is the tetra on any of the blocks?
;(: tetra-overlaps-blocks? (-> Tetra BSet Boolean))
(define (tetra-overlaps-blocks? t bs)
  (not (eq? '() (blocks-intersect (tetra-blocks t) bs))))
