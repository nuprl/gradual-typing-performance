#lang typed/racket/base

(provide node-up-split!)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "label-sublabel.rkt"
  [sublabel (case-> (-> label Index label)
                    (-> label Index Index label))])
(require/typed/check "structs-node-remove-child.rkt"
  [node-remove-child!(-> Node Node Void)])
(require/typed/check "structs-node-add-child.rkt"
  [node-add-child!(-> Node Node Void)])

;; =============================================================================

;; node-up-split!: node number -> node
;; Introduces a new node that goes between this node and its parent.
(: node-up-split! (-> Node Index Node))
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
