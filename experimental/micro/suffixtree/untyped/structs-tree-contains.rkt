#lang racket/base

(provide tree-contains?)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
         "data-suffix-tree.rkt"
(only-in "structs-node-follow-k.rkt"
  node-follow/k))

;; =============================================================================

;; tree-contains?: tree label -> boolean
;; Returns true if the tree contains the given label.
(define (tree-contains? tree label)
  (node-follow/k (suffix-tree-root tree)
                 label
                 (lambda args #t)
                 (lambda args #t)
                 (lambda args #f)
                 (lambda args #f)))
