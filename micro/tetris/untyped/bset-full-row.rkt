#lang racket/base

(provide full-row?)

(require "data-block.rkt")
(require benchmark-util)
(require "consts-board-width.rkt")
(require "bset-blocks-count.rkt")
(require "bset-blocks-row.rkt")

;; =============================================================================

;; Are there a full row of blocks at the given row in the set.
;(: full-row? (-> BSet Natural Boolean))
(define (full-row? bs i)
  (= board-width (blocks-count (blocks-row bs i))))
