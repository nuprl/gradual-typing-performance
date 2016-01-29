#lang typed/racket/base

(provide blocks-overflow?)

(require "data-block-adapted.rkt")

;; =============================================================================

;; Have any of the blocks reach over the top of the board?
(: blocks-overflow? (-> BSet Boolean))
(define (blocks-overflow? bs)
  (ormap (λ: ([b : Block]) (<= (block-y b) 0)) bs))
