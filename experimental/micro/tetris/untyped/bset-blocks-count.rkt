#lang racket/base

(provide blocks-count)

(require "data-block.rkt")

;; =============================================================================

;; Return the number of blocks in the set.
;(: blocks-count (-> BSet Natural))
(define (blocks-count bs)
  (length bs))  ;; No duplicates, cardinality = length.
