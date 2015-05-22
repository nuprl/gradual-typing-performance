#lang racket/base

(provide node-up-splice-leaf!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "structs-node-up-split.rkt"
  node-up-split!)
(only-in "structs-node-add-leaf.rkt"
  node-add-leaf!))

;; =============================================================================

;; node-up-splice-leaf!: node offset label -> (values node node)
;;
;; Adds a new leaf at a splice joint between the node and its
;; parent.  Returns both the joint and the leaf.
(define (node-up-splice-leaf! node offset leaf-label)
  (let* ((split-node (node-up-split! node offset))
         (leaf (node-add-leaf! split-node leaf-label)))
    (values split-node leaf)))
