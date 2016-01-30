#lang racket/base

(provide blocks-subset?)

(require "data-block.rkt"
          benchmark-util)
(require "bset-blocks-contains.rkt")

;; =============================================================================

;; is every element in bs1 also in bs2?
;(: blocks-subset? (-> BSet BSet Boolean))
(define (blocks-subset? bs1 bs2)
  (andmap (λ (b) (blocks-contains? bs2 b)) bs1))
