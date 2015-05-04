#lang typed/racket/base

(provide blocks-rotate-cw)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt")
(require/typed/check "block-block-rotate-cw.rkt"
  [block-rotate-cw (-> Posn Block Block)])

;; =============================================================================

;; Rotate the blocks 90 clockwise around the posn.
(: blocks-rotate-cw (-> Posn BSet BSet))
(define (blocks-rotate-cw c bs)
  (map (λ: ([b : Block]) (block-rotate-cw c b)) bs))
