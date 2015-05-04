#lang typed/racket/base

(provide blocks=?)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "bset-blocks-subset.rkt"
  [blocks-subset? (-> BSet BSet Block)])

;; =============================================================================

;; Determine if given sets of blocks are equal.
(: blocks=? (-> BSet BSet Boolean))
(define (blocks=? bs1 bs2)
  (and (blocks-subset? bs1 bs2)
       (blocks-subset? bs2 bs1)))
