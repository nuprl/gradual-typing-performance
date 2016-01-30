#lang typed/racket/base

(provide blocks-union)

(require "data-block-adapted.rkt")
(require benchmark-util)
(require/typed/check "bset-blocks-contains.rkt"
  [blocks-contains? (-> BSet Block Boolean)])

;; =============================================================================

;; Union the two sets of blocks.
(: blocks-union (-> BSet BSet BSet))
(define (blocks-union bs1 bs2)
  (foldr (λ: ([b  : Block]
              [bs : BSet])
           (cond [(blocks-contains? bs b) bs]
                 [else (cons b bs)]))
         bs2
         bs1))
