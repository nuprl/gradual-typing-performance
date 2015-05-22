#lang typed/racket/base

(provide blocks-change-color)

(require "data-block-adapted.rkt")

;; =============================================================================

(: blocks-change-color (-> BSet Color BSet))
(define (blocks-change-color bs c)
  (map (λ: ([b : Block]) (block (block-x b) (block-y b) c))
       bs))
