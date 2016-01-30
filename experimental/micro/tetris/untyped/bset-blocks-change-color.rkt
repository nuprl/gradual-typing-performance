#lang racket/base

(provide blocks-change-color)

(require "data-block.rkt")

;; =============================================================================

;(: blocks-change-color (-> BSet Color BSet))
(define (blocks-change-color bs c)
  (map (λ (b) (block (block-x b) (block-y b) c))
       bs))
