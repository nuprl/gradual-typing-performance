#lang typed/racket/base

(provide node-remove-child!)

;; -----------------------------------------------------------------------------

(require "data-node-adapted.rkt")

;; =============================================================================

;; node-remove-child!: node node -> void
;; Removes child node.
(: node-remove-child!(-> Node Node Void))
(define (node-remove-child! node child)
  (set-node-children! node (remq child (node-children node))))
