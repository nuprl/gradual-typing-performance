#lang typed/racket

(provide full-row?)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "consts.rkt"
  [board-width Integer])
(require/typed/check "bset-blocks-count.rkt"
  [blocks-count (-> BSet Natural)])
(require/typed/check "bset-blocks-row.rkt"
  [blocks-row (-> BSet Read BSet)])

;; =============================================================================

;; Are there a full row of blocks at the given row in the set.
(: full-row? (-> BSet Natural Boolean))
(define (full-row? bs i)
  (= board-width (blocks-count (blocks-row bs i))))
