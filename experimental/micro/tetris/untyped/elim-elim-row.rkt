#lang racket/base

;; Helper for eliminate-full-rows

(provide elim-row)

(require "data-block.rkt"
         benchmark-util)
(require "bset-blocks-row.rkt")
(require "bset-full-row.rkt")
(require "bset-blocks-union.rkt")
(require "bset-blocks-move.rkt")

;; =============================================================================

;(: elim-row (-> BSet Integer Integer BSet))
(define (elim-row bs i offset)
  (cond [(< i 0) '()]
        [(full-row? bs i)   (elim-row bs (sub1 i) (add1 offset))]
        [else (blocks-union (elim-row bs (sub1 i) offset)
                            (blocks-move 0 offset (blocks-row
                                                   bs i)))]))
