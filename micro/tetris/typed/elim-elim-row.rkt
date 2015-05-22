#lang typed/racket/base

;; Helper for eliminate-full-rows

(provide elim-row)

(require "data-block-adapted.rkt"
         benchmark-util)
(require/typed/check "bset-blocks-row.rkt"
   [blocks-row (-> BSet Real BSet)])
(require/typed/check "bset-full-row.rkt"
   [full-row? (-> BSet Natural Boolean)])
(require/typed/check "bset-blocks-union.rkt"
   [blocks-union (-> BSet BSet BSet)])
(require/typed/check "bset-blocks-move.rkt"
   [blocks-move (-> Real Real BSet BSet)])

;; =============================================================================

(: elim-row (-> BSet Integer Integer BSet))
(define (elim-row bs i offset)
  (cond [(< i 0) '()]
        [(full-row? bs i)   (elim-row bs (sub1 i) (add1 offset))]
        [else (blocks-union (elim-row bs (sub1 i) offset)
                            (blocks-move 0 offset (blocks-row
                                                   bs i)))]))
