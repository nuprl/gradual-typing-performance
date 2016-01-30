#lang racket/base

(provide build-tetra-blocks)

(require benchmark-util
         "data-posn.rkt"
         "data-block.rkt"
         "data-tetra.rkt")
(require "tetras-tetra-move.rkt")

;; =============================================================================

;(: build-tetra-blocks (-> Color Real Real Real Real Real Real Real Real Real Real Tetra))
(define (build-tetra-blocks color xc yc x1 y1 x2 y2 x3 y3 x4 y4)
  (tetra-move 3 0
              (tetra (posn xc yc)
                     (list (block x1 y1 color)
                           (block x2 y2 color)
                           (block x3 y3 color)
                           (block x4 y4 color)))))
