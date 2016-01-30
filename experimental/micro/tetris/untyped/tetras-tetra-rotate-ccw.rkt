#lang racket/base

(provide tetra-rotate-ccw)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt"
         "data-tetra.rkt")
(require "bset-blocks-rotate-ccw.rkt")

;; =============================================================================

;; Rotate the tetra 90 degrees counterclockwise around its center.
;(: tetra-rotate-ccw (-> Tetra Tetra))
(define (tetra-rotate-ccw t)
  (tetra (tetra-center t)
         (blocks-rotate-ccw (tetra-center t)
                            (tetra-blocks t))))
