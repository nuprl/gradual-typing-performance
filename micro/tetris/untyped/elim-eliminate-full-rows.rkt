#lang racket/base

(provide eliminate-full-rows)

(require "data-block.rkt")
(require benchmark-util)
(require "elim-elim-row.rkt")
(require "consts-board-height.rkt")

;; =============================================================================

;; Eliminate all full rows and shift down appropriately.
;(: eliminate-full-rows (-> BSet BSet))
(define (eliminate-full-rows bs)
  (elim-row bs board-height 0))
