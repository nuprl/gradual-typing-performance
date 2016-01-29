#lang typed/racket/base

(provide node-leaf?)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt")

;; =============================================================================

;; node-leaf?: node -> boolean
;; Returns true if node is a leaf
(: node-leaf? (-> Node Boolean))
(define (node-leaf? node)
  (eq? '() (node-children node)))
