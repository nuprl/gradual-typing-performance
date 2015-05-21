#lang racket/base

(provide node-leaf?)

;; -----------------------------------------------------------------------------

(require "data-node.rkt")

;; =============================================================================

;; node-leaf?: node -> boolean
;; Returns true if node is a leaf
(define (node-leaf? node)
  (eq? '() (node-children node)))
