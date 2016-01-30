#lang typed/racket/base

(provide landed-on-floor?)

(require benchmark-util
         "data-block-adapted.rkt"
         "data-world-adapted.rkt"
         "data-tetra-adapted.rkt")
(require/typed/check "tetras-tetra-overlaps-blocks.rkt"
  [tetra-overlaps-blocks? (-> Tetra BSet Boolean)])
(require/typed/check "bset-blocks-max-y.rkt"
  [blocks-max-y (-> BSet Real)])
(require/typed/check "consts-board-height.rkt"
  [board-height Integer])

;; =============================================================================

;; Has the current tetra landed on the floor?
(: landed-on-floor? (-> World Boolean))
(define (landed-on-floor? w)
  (= (blocks-max-y (tetra-blocks (world-tetra w)))
     (sub1 board-height)))
