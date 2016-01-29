#lang typed/racket/base

(provide block=?)

(require "data-block-adapted.rkt")

;; =============================================================================

;; Determines if two blocks are the same (ignoring color).
(: block=? (-> Block Block Boolean))
(define (block=? b1 b2)
  (and (= (block-x b1) (block-x b2))
       (= (block-y b1) (block-y b2))))
