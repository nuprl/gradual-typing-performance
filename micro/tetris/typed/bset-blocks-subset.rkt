#lang typed/racket/base

(provide blocks-subset?)

(require "data-block-adapted.rkt"
          benchmark-util)
(require/typed/check "bset-blocks-contains.rkt"
  [blocks-contains? (-> BSet Block Boolean)])

;; =============================================================================

;; is every element in bs1 also in bs2?
(: blocks-subset? (-> BSet BSet Boolean))
(define (blocks-subset? bs1 bs2)
  (andmap (λ: ([b : Block]) (blocks-contains? bs2 b)) bs1))
