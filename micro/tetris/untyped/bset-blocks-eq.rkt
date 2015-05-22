#lang racket/base

(provide blocks=?)

(require "data-block.rkt")
(require benchmark-util)
(require "bset-blocks-subset.rkt")

;; =============================================================================

;; Determine if given sets of blocks are equal.
;(: blocks=? (-> BSet BSet Boolean))
(define (blocks=? bs1 bs2)
  (and (blocks-subset? bs1 bs2)
       (blocks-subset? bs2 bs1)))
