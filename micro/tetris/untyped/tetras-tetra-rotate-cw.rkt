#lang racket/base

(provide tetra-rotate-cw)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt"
         "data-tetra.rkt")
(require "bset-blocks-rotate-cw.rkt")

;; =============================================================================

;; Rotate the tetra 90 degrees clockwise around its center.
;(: tetra-rotate-cw (-> Tetra Tetra))
(define (tetra-rotate-cw t)
  (tetra (tetra-center t)
         (blocks-rotate-cw (tetra-center t)
                           (tetra-blocks t))))
