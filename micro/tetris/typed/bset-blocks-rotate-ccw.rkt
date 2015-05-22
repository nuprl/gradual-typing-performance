#lang typed/racket/base

(provide blocks-rotate-ccw)

(require benchmark-util
         "data-posn-adapted.rkt"
         "data-block-adapted.rkt")
(require/typed/check "block-block-rotate-ccw.rkt"
  [block-rotate-ccw (-> Posn Block Block)])

;; =============================================================================

;; Rotate the blocks 90 counterclockwise around the posn.
(: blocks-rotate-ccw (-> Posn BSet BSet))
(define (blocks-rotate-ccw c bs)
  (map (λ: ([b : Block]) (block-rotate-ccw c b)) bs))
