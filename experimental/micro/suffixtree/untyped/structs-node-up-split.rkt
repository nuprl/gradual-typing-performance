#lang racket/base

(provide node-up-split!)

;; -----------------------------------------------------------------------------

(require "data-node.rkt"
         "data-label.rkt"
(only-in "label-sublabel.rkt"
  sublabel)
(only-in "structs-node-remove-child.rkt"
  node-remove-child!)
(only-in "structs-node-add-child.rkt"
  node-add-child!))

;; =============================================================================

;; node-up-split!: node number -> node
;; Introduces a new node that goes between this node and its parent.
(define (node-up-split! nd offset)
  (let* ((label (node-up-label nd))
         (pre-label (sublabel label 0 offset))
         (post-label (sublabel label offset))
         (parent (node-parent nd))
         (new-node (node pre-label parent (list nd) #f)))
    (set-node-up-label! nd post-label)
    (unless parent (error "node-up-split!"))
    (node-remove-child! parent nd)
    (set-node-parent! nd new-node)
    (node-add-child! parent new-node)
    new-node))
