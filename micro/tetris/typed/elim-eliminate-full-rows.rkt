#lang typed/racket/base

(provide eliminate-full-rows)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "elim-elim-row.rkt"
  [elim-row (-> BSet Integer Integer BSet)])
(require/typed/check "consts-board-height.rkt"
  [board-height Integer])

;; =============================================================================

;; Eliminate all full rows and shift down appropriately.
(: eliminate-full-rows (-> BSet BSet))
(define (eliminate-full-rows bs)
  (elim-row bs board-height 0))
