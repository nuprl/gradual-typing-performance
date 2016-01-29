#lang racket/base

(provide tetra-change-color)

(require benchmark-util
         "data-block.rkt"
         "data-tetra.rkt")
(require "bset-blocks-change-color.rkt")

;; =============================================================================

;; Change the color of the given tetra.
;(: tetra-change-color (-> Tetra Color Tetra))
(define (tetra-change-color t c)
  (tetra (tetra-center t)
         (blocks-change-color (tetra-blocks t) c)))
