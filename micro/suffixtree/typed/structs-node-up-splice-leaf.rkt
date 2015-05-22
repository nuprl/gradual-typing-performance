#lang typed/racket/base

(provide node-up-splice-leaf!)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt"
         "data-label-adapted.rkt"
         benchmark-util)
(require/typed/check "structs-node-up-split.rkt"
  [node-up-split! (-> Node Index Node)])
(require/typed/check "structs-node-add-leaf.rkt"
  [node-add-leaf! (-> Node Label Node)])

;; =============================================================================

;; node-up-splice-leaf!: node offset label -> (values node node)
;;
;; Adds a new leaf at a splice joint between the node and its
;; parent.  Returns both the joint and the leaf.
(: node-up-splice-leaf! (-> Node Index Label (values Node Node)))
(define (node-up-splice-leaf! node offset leaf-label)
  (let* ((split-node (node-up-split! node offset))
         (leaf (node-add-leaf! split-node leaf-label)))
    (values split-node leaf)))
