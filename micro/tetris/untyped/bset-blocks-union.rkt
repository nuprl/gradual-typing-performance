#lang racket/base

(provide blocks-union)

(require "data-block.rkt")
(require benchmark-util)
(require "bset-blocks-contains.rkt")

;; =============================================================================

;; Union the two sets of blocks.
;(: blocks-union (-> BSet BSet BSet))
(define (blocks-union bs1 bs2)
  (foldr (λ (b
              bs)
           (cond [(blocks-contains? bs b) bs]
                 [else (cons b bs)]))
         bs2
         bs1))
